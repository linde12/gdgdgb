use crate::{
    display::Display,
    mmu::{Interrupt, Mmu},
};

#[derive(Debug)]
enum PpuState {
    OAMSearch,
    PixelTransfer,
    HBlank,
    VBlank,
}

pub static SCALE: usize = 4;
pub static XRES: usize = 160; // Horizontal resolution
pub static YRES: usize = 144; // Vertical resolution
static SCANLINE_CYCLES: u16 = 456; // Total cycles per scanline
static OAM_CYCLES: u16 = 80; // Cycles for OAM search
static PIXEL_TRANSFER_CYCLES: u16 = 172; // Cycles for pixel transfer
static HBLANK_CYCLES: u16 = 204; // Cycles for HBlank

#[derive(Debug)]
enum LcdRegister {
    Lcdc = 0xFF40,
    Stat = 0xFF41,
    Scy = 0xFF42,
    Scx = 0xFF43,
    Ly = 0xFF44,
    Lyc = 0xFF45,
    Bgp = 0xFF47,
    Wy = 0xFF4A,
    Wx = 0xFF4B,
}

impl From<LcdRegister> for u16 {
    fn from(reg: LcdRegister) -> Self {
        reg as u16
    }
}

pub struct PPU<D: Display> {
    pub cycle: u16, // internal counter
    state: PpuState,
    display: D,
    scanline_rendered: bool,
    frame_ready: bool,
}

impl<D: Display> PPU<D> {
    pub fn new(_mmu: &mut Mmu, mut display: D) -> Self {
        display.enable();
        Self {
            cycle: 0,
            state: PpuState::OAMSearch,
            display,
            scanline_rendered: false,
            frame_ready: false,
        }
    }

    /// Returns true and clears the flag if a frame has just been completed
    /// since the last call.
    pub fn take_frame_ready(&mut self) -> bool {
        let r = self.frame_ready;
        self.frame_ready = false;
        r
    }

    /// Called every CPU cycle (or every N cycles depending on your CPU timing)
    pub fn step(&mut self, cycles: u8, mmu: &mut Mmu) {
        // If LCD is disabled (LCDC bit 7 == 0), keep PPU parked at LY=0/OAMSearch.
        let lcdc = mmu.byte(LcdRegister::Lcdc.into());
        if lcdc & 0x80 == 0 {
            self.cycle = 0;
            self.state = PpuState::OAMSearch;
            self.scanline_rendered = false;
            mmu.write_byte(LcdRegister::Ly.into(), 0);
            return;
        }

        self.cycle += cycles as u16;

        match self.state {
            PpuState::OAMSearch => {
                if self.cycle >= OAM_CYCLES {
                    self.cycle -= OAM_CYCLES;
                    self.state = PpuState::PixelTransfer;
                    self.scanline_rendered = false;
                }
            }

            PpuState::PixelTransfer => {
                // Render the entire scanline once when we enter this mode.
                if !self.scanline_rendered {
                    let ly = mmu.byte(LcdRegister::Ly.into());
                    self.render_scanline(mmu, ly);
                    self.scanline_rendered = true;
                }
                if self.cycle >= PIXEL_TRANSFER_CYCLES {
                    self.cycle -= PIXEL_TRANSFER_CYCLES;
                    self.state = PpuState::HBlank;
                }
            }

            PpuState::HBlank => {
                if self.cycle >= HBLANK_CYCLES {
                    self.cycle -= HBLANK_CYCLES;
                    self.display.hblank();

                    // Advance LY (current line)
                    let ly = mmu.byte(LcdRegister::Ly.into()).wrapping_add(1);
                    mmu.write_byte(LcdRegister::Ly.into(), ly);

                    if ly >= 144 {
                        // Enter VBlank
                        self.state = PpuState::VBlank;

                        // Request VBlank interrupt
                        mmu.request_interrupt(Interrupt::VBlank);
                        self.display.vblank();
                    } else {
                        self.state = PpuState::OAMSearch;
                    }
                }
            }

            PpuState::VBlank => {
                if self.cycle >= SCANLINE_CYCLES {
                    self.cycle -= SCANLINE_CYCLES;

                    // Advance LY (current line)
                    let ly = mmu.byte(LcdRegister::Ly.into()).wrapping_add(1);
                    mmu.write_byte(LcdRegister::Ly.into(), ly);

                    if ly >= 154 {
                        // Restart scanning from line 0
                        mmu.write_byte(LcdRegister::Ly.into(), 0);
                        self.state = PpuState::OAMSearch;

                        self.display.draw();
                        self.frame_ready = true;
                    }
                }
            }
        }
    }

    /// Render one full BG scanline into the framebuffer.
    fn render_scanline(&mut self, mmu: &mut Mmu, ly: u8) {
        if ly >= 144 {
            return;
        }

        let lcdc = mmu.byte(LcdRegister::Lcdc.into());
        let bg_enabled = lcdc & 0x01 != 0;

        // If BG is disabled, output white (color 0) for the whole line.
        if !bg_enabled {
            for x in 0..160u8 {
                self.display.write(x, ly, 0);
            }
            return;
        }

        let scy = mmu.byte(LcdRegister::Scy.into());
        let scx = mmu.byte(LcdRegister::Scx.into());
        let bgp = mmu.byte(LcdRegister::Bgp.into());

        // LCDC bit 3: BG tile map area (0=$9800, 1=$9C00)
        let bg_map_base: u16 = if lcdc & 0x08 != 0 { 0x9C00 } else { 0x9800 };
        // LCDC bit 4: BG/Window tile data area (1=$8000 unsigned, 0=$8800 signed)
        let unsigned_addressing = lcdc & 0x10 != 0;

        let bg_y = scy.wrapping_add(ly);
        let tile_row = (bg_y / 8) as u16;
        let line_in_tile = (bg_y % 8) as u16;

        for x in 0..160u8 {
            let bg_x = scx.wrapping_add(x);
            let tile_col = (bg_x / 8) as u16;
            let bit_in_tile = 7 - (bg_x % 8);

            // Fetch tile index from the BG map.
            let map_addr = bg_map_base + tile_row * 32 + tile_col;
            let tile_id = mmu.byte(map_addr);

            // Compute address of tile data for this line.
            let tile_data_addr = if unsigned_addressing {
                0x8000u16 + (tile_id as u16) * 16 + line_in_tile * 2
            } else {
                // signed: base is $9000, tile_id treated as i8
                let signed = tile_id as i8 as i32;
                (0x9000i32 + signed * 16 + (line_in_tile as i32) * 2) as u16
            };

            let low = mmu.byte(tile_data_addr);
            let high = mmu.byte(tile_data_addr + 1);

            let lo_bit = (low >> bit_in_tile) & 1;
            let hi_bit = (high >> bit_in_tile) & 1;
            let color_id = (hi_bit << 1) | lo_bit;

            // Apply BGP palette: bits [1:0]=color 0, [3:2]=1, [5:4]=2, [7:6]=3.
            let shade = (bgp >> (color_id * 2)) & 0x3;

            self.display.write(x, ly, shade);
        }
    }
}

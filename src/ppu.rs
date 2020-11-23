use crate::mmu::Mmu;

const SCREEN_WIDTH_PX: usize = 160;
const SCREEN_HEIGHT_PX: usize = 144;

// http://bgb.bircd.org/pandocs.htm#videodisplay
// Bit 7 - LCD Display Enable             (0=Off, 1=On)
// Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
// Bit 5 - Window Display Enable          (0=Off, 1=On)
// Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
// Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
// Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
// Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
// Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)
const LCDC_ADDRESS: usize = 0xFF40; // Adress of the LCD Control register in HRAM
const STAT_ADDRESS: usize = 0xFF41; // Adress of the LCD Control register in HRAM
const LY_ADDRESS: u16 = 0xFF44; // Adress of the LCD Control register in HRAM

const SCANLINE_OAM_ACCESS_CYCLES: u32 = 80;
const SCANLINE_VRAM_ACCESS_CYCLES: u32 = 172;
const SCANLINE_HBLANK_CYCLES: u32 = 204;
const SCANLINE_TOTAL_CYCLES: u8 = 456;

const VBLANK_CYCLES: u32 = 4560;

const LY_VBLANK_OFFSET: u8 = 154;

// TODO: Maybe useful later
// const CLOCK_SPEED: u32 = 4194304;
// const FPS: u32 = 60;
// const CYCLES_PER_FRAME: u32 = CLOCK_SPEED / FPS;

// TODO: Might be useful if we want to map bits to an enum
// enum Mode {
//     HBlank,
//     VBlank,
//     OAMScan,
//     HDraw,
// }

pub struct Ppu<'a> {
    // TODO: Remove? This resides in memory, but it is set to 0x80 by boot rom?
    // ly: u8, // current scanline, 0-144 (or 0-154 if including the VBLANK period)
    scanline_counter: u8,
    mmu: &'a mut Mmu,
    screen_buffer: [[[u8; 3]; SCREEN_HEIGHT_PX]; SCREEN_WIDTH_PX]
}

impl<'a> Ppu<'a> {
    pub fn new(mmu: &'a mut Mmu) -> Ppu {
        Ppu {
            // TODO: Remove? This resides in memory, but it is set to 0x80 by boot rom?
            // ly: 0x80, // MSB is always set to 1
            scanline_counter: 0,
            mmu,
            screen_buffer: [[[0; 3]; SCREEN_HEIGHT_PX]; SCREEN_WIDTH_PX],
        }
    }

    pub fn step(&mut self, cycles: u8) {
        let ly = self.mmu.byte(LY_ADDRESS);
        self.scanline_counter += cycles;

        if self.scanline_counter >= SCANLINE_TOTAL_CYCLES {
            let next_ly = ly + 1;
            self.mmu.write_byte(LY_ADDRESS, next_ly); // increase LY
            if next_ly == LY_VBLANK_OFFSET { // reset LY to 0 once we're done with VBLANK
                self.mmu.write_byte(LY_ADDRESS, 0);
            }

            self.scanline_counter = self.scanline_counter % SCANLINE_TOTAL_CYCLES;
            // TODO: Or just this? Instructions that are e.g. 8 cycles will potentionally
            // "overflow". Ideally we should call `step` every 4 cycles instead, but that
            // would require a redesign of how the CPU works...
            // self.scanline_counter = 0;

            if next_ly == LY_VBLANK_OFFSET {
                // TODO: Interrupt VBLANK
            }
        }
    }
}

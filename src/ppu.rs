use crate::mmu::Mmu;

pub struct PPU {
    pub cycle: u16, // internal counter
}

impl PPU {
    pub fn new(mmu: &mut Mmu) -> Self {
        mmu.write_byte(0xFF40, 0x91); // LCDC
        mmu.write_byte(0xFF41, 0x85); // STAT
        mmu.write_byte(0xFF42, 0x00); // SCY
        mmu.write_byte(0xFF43, 0x00); // SCX
        mmu.write_byte(0xFF44, 0x00); // LY
        mmu.write_byte(0xFF45, 0x00); // LYC
        mmu.write_byte(0xFF46, 0x00); // DMA (not handled here)
        mmu.write_byte(0xFF47, 0xFC); // BGP
        mmu.write_byte(0xFF48, 0xFF); // OBP0
        mmu.write_byte(0xFF49, 0xFF); // OBP1
        mmu.write_byte(0xFF4A, 0x00); // WY
        mmu.write_byte(0xFF4B, 0x00); // WX

        Self { cycle: 0 }
    }

    /// Called every CPU cycle (or every N cycles depending on your CPU timing)
    pub fn step(&mut self, cycles: u8, mmu: &mut Mmu) {
        self.cycle += cycles as u16;

        // Advance LY every 456 cycles (~1 scanline)
        if self.cycle >= 456 {
            self.cycle -= 456;
            let ly = mmu.byte(0xFF44);
            let ly = ly.wrapping_add(1);
            mmu.write_byte(0xFF44, ly); // LY
            if ly > 153 {
                mmu.write_byte(0xFF44, 0); // LY
            }
        }

        // Update STAT LY==LYC flag
        let ly = mmu.byte(0xFF44);
        let lyc = mmu.byte(0xFF45);
        if ly == lyc {
            let mut stat = mmu.byte(0xFF41);
            stat |= 0b0000_0100; // set coincidence flag
            mmu.write_byte(0xFF41, stat);
        } else {
            let mut stat = mmu.byte(0xFF41);
            stat &= !0b0000_0100; // clear coincidence flag
            mmu.write_byte(0xFF41, stat);
        }
    }
}

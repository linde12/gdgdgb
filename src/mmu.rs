use crate::error::GBError;

// See https://gbdev.gg8.se/wiki/articles/Memory_Map
pub struct Mmu {
    rom: [u8; 0x8000], // consists of 32k, lower 16k is static while the uppe 16k is a switchable rom bank from cartridge
    vram: [u8; 0x2000],
    ext_ram: [u8; 0x2000], // external ram from cartridge
    ram: [u8; 0x2000], // working ram
    oam: [u8; 0xA0], // "Object Attribute Memory" or sprite memory, sprites are stored here
    io: [u8; 0x80], // "Object Attribute Memory" or sprite memory, sprites are stored here
    hram: [u8; 0x7F], // "High RAM", fast ram on the gameboy(?)
    ie: u8 // interrupt enable register
}

impl Mmu {
    pub fn new() -> Mmu {
        Mmu {
            rom: [0u8; 0x8000],
            vram: [0u8; 0x2000],
            ext_ram: [0u8; 0x2000],
            ram: [0u8; 0x2000],
            oam: [0u8; 0xA0],
            io: [0u8; 0x80],
            hram: [0u8; 0x7F],
            ie: 0,
        }
    }

    pub fn byte(&self, index: usize) -> u8 {
        match index {
            0x0000 ..= 0x7FFF => self.rom[index],
            0x8000 ..= 0x9FFF => self.vram[index - 0x8000],
            0xA000 ..= 0xBFFF => self.ext_ram[index - 0xA000],
            0xC000 ..= 0xDFFF => self.ram[index - 0xC000],
            0xFE00 ..= 0xFE9F => self.oam[index - 0xFE00],
            0xFF00 ..= 0xFF7F => self.io[index - 0xFF00],
            0xFF80 ..= 0xFFFE => self.hram[index - 0xFF80],
            0xFFFF => self.ie,
            _ => 0xFF, // default to 0xFF
        }
    }

    pub fn word(&self, index: usize) -> usize {
        let first = self.byte(index);
        let second = self.byte(index + 1);

        // little-endian, least significant bit comes first, hence | and << 8
        (first as usize) | ((second as usize) << 8)
    }

    pub fn write_byte(&mut self, index: usize, b: u8) -> Result<(), GBError> {
        match index {
            0x0000 ..= 0x7FFF => {self.rom[index] = b; Ok(())},
            0x8000 ..= 0x9FFF => {self.vram[index - 0x8000] = b; Ok(())},
            0xA000 ..= 0xBFFF => {self.ext_ram[index - 0xA000] = b; Ok(())},
            0xC000 ..= 0xDFFF => {self.ram[index - 0xC000] = b; Ok(())},
            0xFE00 ..= 0xFE9F => {self.oam[index - 0xFE00] = b; Ok(())},
            0xFF00 ..= 0xFF7F => {self.io[index - 0xFF00] = b; Ok(())},
            0xFF80 ..= 0xFFFE => {self.hram[index - 0xFF80] = b; Ok(())},
            0xFFFF => {self.ie = b; Ok(())},
            _ => Err(GBError::WriteByte),
        }
    }

    pub fn write_word(&mut self, index: usize, w: usize) -> Result<(), GBError> {
        self.write_byte(index, (w & 0xFF) as u8)?;
        self.write_byte(index, (w >> 8) as u8)?;
        Ok(())
    }

    pub fn load_rom(&mut self, rom: Vec<u8>) -> Result<(), GBError> {
        if rom.len() > 4096 {
            return Err(GBError::LoadRom("rom size too large".into()));
        }

        // TODO: Revisit later, nicer way?
        for (i, item) in rom.iter().enumerate() {
            self.write_byte(i, *item);
        }
        Ok(())
    }

    pub fn clear_vram(&mut self) {
        self.vram = [0; 0x2000];
    }
}
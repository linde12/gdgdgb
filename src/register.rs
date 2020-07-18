#[derive(Debug, Copy, Clone)]
pub enum RegisterType8 {
    A,
    B,
    C,
    D,
    E,
    F, // Holds CPU Flags
    H,
    L,
}

#[derive(Debug, Copy, Clone)]
pub enum RegisterType16 {
    AF,
    BC,
    DE,
    HL,
    HLD, // Not an actual register, HL Decrement (HL-)
    HLI, // Not an actual register, HL Increment (HL+)
    SP,
}

#[derive(Debug, Copy, Clone)]
pub enum Flag {
    Zero,
    Negative,
    HalfCarry,
    Carry,
}


#[derive(Debug)]
pub enum ByteOrWord {
    Byte(u8),
    Word(usize),
}

impl From<u8> for ByteOrWord {
    fn from(b: u8) -> Self {
        ByteOrWord::Byte(b)
    }
}

impl From<usize> for ByteOrWord {
    fn from(w: usize) -> Self {
        ByteOrWord::Word(w)
    }
}

#[derive(Debug)]
pub struct FlagsRegister {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
}

impl From<u8> for FlagsRegister {
    fn from(flags: u8) -> Self {
        let flags = flags >> 4;

        let z = flags >> 3 == 1;
        let n = flags & 0b0100 >> 2 == 1;
        let h = flags & 0b0010 >> 1 == 1;
        let c = flags & 0b0001 == 1;

        FlagsRegister { z, n, h, c }
    }
}

pub struct Register {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: u8,
    pub h: u8,
    pub l: u8,
    pub pc: usize,
    pub sp: usize,
}

impl Register {
    pub fn new() -> Self {
        Register {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: 0,
            h: 0,
            l: 0,
            pc: 0,
            sp: 0,
        }
    }

    pub fn set_reg16(&mut self, reg: RegisterType16, value: usize) {
        match reg {
            // TODO: Store F register as bits instead of a struct
            RegisterType16::AF => { self.a = (value & 0x00FF) as u8; self.f = (value & 0xFF00 >> 8) as u8; }
            RegisterType16::BC => { self.b = (value & 0x00FF) as u8; self.c = (value & 0xFF00 >> 8) as u8; }
            RegisterType16::DE => { self.d = (value & 0x00FF) as u8; self.e = (value & 0xFF00 >> 8) as u8; }
            RegisterType16::HL => { self.h = (value & 0x00FF) as u8; self.l = (value & 0xFF00 >> 8) as u8; }
            RegisterType16::HLD => { self.h = (value & 0x00FF) as u8; self.l = (value & 0xFF00 >> 8) as u8; }
            RegisterType16::HLI => { self.h = (value & 0x00FF) as u8; self.l = (value & 0xFF00 >> 8) as u8; }
            RegisterType16::SP => self.sp = value,
            // TODO: Refactor HLD/HLI to not be a RegisterType?
            _ => panic!("cannot set 8 bit regs or HLD, HLI with set_reg16"),
        }
    }

    pub fn reg16(&self, reg: RegisterType16) -> usize {
        match reg {
            // TODO: Correct endianess?
            RegisterType16::AF => (((self.a as u16) << 8) | self.f as u16) as usize,
            RegisterType16::BC => (((self.b as u16) << 8) | self.c as u16) as usize,
            RegisterType16::DE => (((self.d as u16) << 8) | self.e as u16) as usize,
            RegisterType16::HL => self.hl(),
            RegisterType16::SP => self.sp,
            // RegisterType16::HLD => {let v = self.hl(); self.dec_hl(); v }
            // RegisterType16::HLI => {let v = self.hl(); self.inc_hl(); v }
            _ => panic!("cannot get 8 bit regs or HLD, HLI with reg16"),
        }
    }

    pub fn dec_hl(&mut self) {
        let new_hl = self.hl() - 1;
        self.set_reg16(RegisterType16::HL, new_hl);
    }

    pub fn inc_hl(&mut self) {
        let new_hl = self.hl() + 1;
        self.set_reg16(RegisterType16::HL, new_hl);
    }

    fn hl(&self) -> usize {
        (((self.h as u16) << 8) | self.l as u16) as usize
    }

    pub fn set_reg8(&mut self, reg: RegisterType8, value: u8) {
        match reg {
            RegisterType8::A => self.a = value,
            RegisterType8::B => self.b = value,
            RegisterType8::C => self.c = value,
            RegisterType8::D => self.d = value,
            RegisterType8::E => self.e = value,
            RegisterType8::F => self.f = value,
            RegisterType8::H => self.h = value,
            RegisterType8::L => self.l = value,
        }
    }

    pub fn reg8(&self, reg: RegisterType8) -> u8 {
        match reg {
            RegisterType8::A => self.a,
            RegisterType8::B => self.b,
            RegisterType8::C => self.c,
            RegisterType8::D => self.d,
            RegisterType8::E => self.e,
            RegisterType8::F => self.f,
            RegisterType8::H => self.h,
            RegisterType8::L => self.l,
        }
    }

    pub fn set_flag(&mut self, flag: Flag, value: bool) {
        match flag {
            Flag::Zero => self.f |= 0b1000_0000,
            Flag::Negative => self.f |= 0b0100_0000,
            Flag::HalfCarry => self.f |= 0b0010_0000,
            Flag::Carry => self.f |= 0b0001_0000,
        }
    }
}

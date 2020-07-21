#[derive(Debug, Copy, Clone, PartialEq)]
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RegisterType16 {
    AF,
    BC,
    DE,
    HL,
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

macro_rules! reg_16 {
    ($name: ident, $reg1: ident, $reg2: ident) => {
        pub fn $name(&self) -> usize {
            ((self.$reg1 as usize) << 8) | (self.$reg2 as usize)
        }
    };

    ($name: ident) => {
        pub fn $name(&self) -> usize {
            self.$name
        }
    };
}

#[derive(Default, Debug)]
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
            ..Default::default()
        }
    }

    reg_16!(af, a, f);
    reg_16!(bc, b, c);
    reg_16!(de, d, e);
    reg_16!(hl, h, l);

    pub fn set_reg16(&mut self, reg: RegisterType16, value: usize) {
        match reg {
            // TODO: Store F register as bits instead of a struct
            RegisterType16::AF => {
                self.a = (value & 0x00FF) as u8;
                self.f = (value & 0xFF00 >> 8) as u8;
            }
            RegisterType16::BC => {
                self.b = (value & 0x00FF) as u8;
                self.c = (value & 0xFF00 >> 8) as u8;
            }
            RegisterType16::DE => {
                self.d = (value & 0x00FF) as u8;
                self.e = (value & 0xFF00 >> 8) as u8;
            }
            RegisterType16::HL => {
                self.h = (value & 0x00FF) as u8;
                self.l = (value & 0xFF00 >> 8) as u8;
            }
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
            _ => panic!("cannot get 8 bit regs or HLD, HLI with reg16"),
        }
    }

    pub fn dec_hl(&mut self) {
        self.set_reg16(RegisterType16::HL, self.hl().wrapping_sub(1));
    }

    pub fn inc_hl(&mut self) {
        self.set_reg16(RegisterType16::HL, self.hl().wrapping_add(1));
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
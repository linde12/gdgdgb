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
        let z = flags >> 7 == 1;
        let n = (flags & 0b0100_0000) >> 6 == 1;
        let h = (flags & 0b0010_0000) >> 5 == 1;
        let c = (flags & 0b0001_0000) >> 4 == 1;

        FlagsRegister { z, n, h, c }
    }
}

macro_rules! reg {
    ($name: ident, $reg1: ident, $reg2: ident) => {
        pub fn $name(&self) -> u16 {
            ((self.$reg1 as u16) << 8) | (self.$reg2 as u16)
        }
    };
}

macro_rules! set_reg {
    ($name: ident, $reg1: ident, $reg2: ident) => {
        pub fn $name(&mut self, value: u16) {
            let [hi, lo] = value.to_be_bytes();
            self.$reg1 = hi;
            self.$reg2 = lo;
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
}

impl Register {
    pub fn new() -> Self {
        Register {
            f: 0b1000_0000,
            ..Default::default()
        }
    }

    reg!(af, a, f);
    reg!(bc, b, c);
    reg!(de, d, e);
    reg!(hl, h, l);
    set_reg!(set_af, a, f);
    set_reg!(set_bc, b, c);
    set_reg!(set_de, d, e);
    set_reg!(set_hl, h, l);

    pub fn set_flag(&mut self, flag: Flag, value: bool) {
        if value == true {
            match flag {
                Flag::Zero => self.f |= 0b1000_0000,
                Flag::Negative => self.f |= 0b0100_0000,
                Flag::HalfCarry => self.f |= 0b0010_0000,
                Flag::Carry => self.f |= 0b0001_0000,
            }
        } else {
            match flag {
                Flag::Zero => self.f &= 0b0111_1111,
                Flag::Negative => self.f &= 0b1011_1111,
                Flag::HalfCarry => self.f &= 0b1101_1111,
                Flag::Carry => self.f &= 0b1110_1111,
            }
        }
    }
    pub fn flag(&self, flag: Flag) -> bool {
        let flags: FlagsRegister = self.f.into();
        match flag {
            Flag::Zero => flags.z,
            Flag::Negative => flags.n,
            Flag::HalfCarry => flags.h,
            Flag::Carry => flags.c,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_flag() {
        let regs = Register::new();
        assert!(regs.f == 0);

        let mut regs = Register::new();
        regs.set_flag(Flag::Zero, true);
        assert!(regs.f == 0x80);

        let mut regs = Register::new();
        regs.set_flag(Flag::Negative, true);
        assert!(regs.f == 0x40);

        let mut regs = Register::new();
        regs.set_flag(Flag::HalfCarry, true);
        assert!(regs.f == 0x20);

        let mut regs = Register::new();
        regs.set_flag(Flag::Carry, true);
        assert!(regs.f == 0x10);
    }

    #[test]
    fn get_flag() {
        let mut regs = Register::new();
        regs.set_flag(Flag::Zero, true);
        let flags: FlagsRegister = regs.f.into();
        assert!(flags.z == true);

        let mut regs = Register::new();
        regs.set_flag(Flag::Negative, true);
        let flags: FlagsRegister = regs.f.into();
        assert!(flags.n == true);

        let mut regs = Register::new();
        regs.set_flag(Flag::HalfCarry, true);
        let flags: FlagsRegister = regs.f.into();
        assert!(flags.h == true);

        let mut regs = Register::new();
        regs.set_flag(Flag::Carry, true);
        let flags: FlagsRegister = regs.f.into();
        assert!(flags.c == true);
    }
}

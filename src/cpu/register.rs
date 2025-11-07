use std::fmt::Display;

#[derive(Debug, Copy, Clone)]
pub enum Flag {
    Zero,
    Negative,
    HalfCarry,
    Carry,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Flags(pub u8);

impl Flags {
    /// Set a specific flag to true or false
    pub fn set(&mut self, flag: Flag, value: bool) {
        let mask = match flag {
            Flag::Zero => 0b1000_0000,
            Flag::Negative => 0b0100_0000,
            Flag::HalfCarry => 0b0010_0000,
            Flag::Carry => 0b0001_0000,
        };
        if value {
            self.0 |= mask;
        } else {
            self.0 &= !mask;
        }
    }

    /// Read a specific flag
    pub fn get(&self, flag: Flag) -> bool {
        let mask = match flag {
            Flag::Zero => 0b1000_0000,
            Flag::Negative => 0b0100_0000,
            Flag::HalfCarry => 0b0010_0000,
            Flag::Carry => 0b0001_0000,
        };
        (self.0 & mask) != 0
    }
}

impl Display for Flags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Z:{} N:{} H:{} C:{}",
            self.get(Flag::Zero) as bool,
            self.get(Flag::Negative) as bool,
            self.get(Flag::HalfCarry) as bool,
            self.get(Flag::Carry) as bool
        )
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
    pub f: Flags,
    pub h: u8,
    pub l: u8,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AF: {:04X}  BC: {:04X}  DE: {:04X}  HL: {:04X}",
            self.af(),
            self.bc(),
            self.de(),
            self.hl()
        )
    }
}

impl Register {
    pub fn new() -> Self {
        Register {
            f: Flags(0b0000_0000),
            ..Default::default()
        }
    }

    pub fn af(&self) -> u16 {
        ((self.a as u16) << 8) | (self.f.0 as u16)
    }

    pub fn set_af(&mut self, value: u16) {
        let [hi, lo] = value.to_be_bytes();
        self.a = hi;
        self.f.0 = lo & 0b1111_0000; // lower nibble of F is always 0
    }

    reg!(bc, b, c);
    reg!(de, d, e);
    reg!(hl, h, l);
    set_reg!(set_bc, b, c);
    set_reg!(set_de, d, e);
    set_reg!(set_hl, h, l);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_flag() {
        let regs = Register::new();
        assert_eq!(regs.f.0, 0);

        let mut regs = Register::new();
        regs.f.set(Flag::Zero, true);
        assert_eq!(regs.f.0, 0x80);

        let mut regs = Register::new();
        regs.f.set(Flag::Negative, true);
        assert_eq!(regs.f.0, 0x40);
        // assert!(regs.f.0 == 0x40);

        let mut regs = Register::new();
        regs.f.set(Flag::HalfCarry, true);
        assert_eq!(regs.f.0, 0x20);

        let mut regs = Register::new();
        regs.f.set(Flag::Carry, true);
        assert_eq!(regs.f.0, 0x10);
    }

    #[test]
    fn get_flag() {
        let mut regs = Register::new();
        regs.f.set(Flag::Zero, true);
        assert!(regs.f.get(Flag::Zero) == true);

        let mut regs = Register::new();
        regs.f.set(Flag::Negative, true);
        assert!(regs.f.get(Flag::Negative) == true);

        let mut regs = Register::new();
        regs.f.set(Flag::HalfCarry, true);
        assert!(regs.f.get(Flag::HalfCarry) == true);

        let mut regs = Register::new();
        regs.f.set(Flag::Carry, true);
        assert!(regs.f.get(Flag::Carry) == true);
    }
}

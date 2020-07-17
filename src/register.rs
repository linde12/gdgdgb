#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum RegisterType {
    A,
    B,
    C,
    D,
    E,
    F, // Holds CPU Flags
    H,
    L,
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
    NZ, // Not Zero
    NC, // No Carry
    Z,  // Zero
    C,  // Carry
}

pub struct FlagsRegister {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
}

impl FlagsRegister {
    fn new() -> Self {
        FlagsRegister {
            z: false,
            n: false,
            h: false,
            c: false,
        }
    }
}

impl From<&FlagsRegister> for u8 {
    fn from(flags: &FlagsRegister) -> Self {
        let mut n: u8 = 0;
        if flags.z {
            n = n & 0b1000
        }
        if flags.n {
            n = n & 0b0100
        }
        if flags.h {
            n = n & 0b0010
        }
        if flags.c {
            n = n & 0b0001
        }

        n << 4
    }
}

pub struct Register {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: FlagsRegister,
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
            f: FlagsRegister::new(),
            h: 0,
            l: 0,
            pc: 0,
            sp: 0,
        }
    }
}

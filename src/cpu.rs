use crate::error::GBError;
use crate::mmu::Mmu;

const IO_REGISTER_OFFSET: u16 = 0xff00;

// Used to algorithmically parse opcode ranges
static DEFAULT_DST_REGISTER_ORDER: [Destination; 8] = [
    Destination::Direct(Target::Register(Register::B)),
    Destination::Direct(Target::Register(Register::C)),
    Destination::Direct(Target::Register(Register::D)),
    Destination::Direct(Target::Register(Register::E)),
    Destination::Direct(Target::Register(Register::H)),
    Destination::Direct(Target::Register(Register::L)),
    Destination::Indirect(Target::Register(Register::HL)), // (HL)
    Destination::Direct(Target::Register(Register::A)),
];

static DEFAULT_SRC_REGISTER_ORDER: [Source; 8] = [
    Source::Direct(Target::Register(Register::B)),
    Source::Direct(Target::Register(Register::C)),
    Source::Direct(Target::Register(Register::D)),
    Source::Direct(Target::Register(Register::E)),
    Source::Direct(Target::Register(Register::H)),
    Source::Direct(Target::Register(Register::L)),
    Source::Indirect(Target::Register(Register::HL)), // (HL)
    Source::Direct(Target::Register(Register::A)),
];

#[derive(Debug)]
pub enum ProgramCounter {
    Next,
    Jump(usize),
}

// See https://www.cs.helsinki.fi/u/kerola/tito/koksi_doc/memaddr.html
#[derive(Debug, Copy, Clone)]
pub enum Destination {
    Direct(Target),       // Direct value, either a register or u16 address
    Indirect(Target),     // A pointer to an address, either from register or an address location
    Indexed(Target, u16), // Value of target+offset, where target can be a value in a register or a u16 and offset is a u16
}

// See https://www.cs.helsinki.fi/u/kerola/tito/koksi_doc/memaddr.html
#[derive(Debug, Copy, Clone)]
pub enum Source {
    Immediate8(u8),
    Immediate16(u16),
    Direct(Target),       // Direct value, either a register or u16 address
    Indirect(Target),     // A pointer to an address, either from register or an address location
    Indexed(Target, u16), // Value of target+offset, where target can be a value in a register or a u16 and offset is a u16
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum Register {
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
pub enum Target {
    Register(Register),
    Address(u16),
}

#[derive(Debug)]
pub enum Op {
    NOP,
    PREFIX,                  // 0xCB
    LD(Destination, Source), // Load Operand 2 into Operand 1
    ADD(Destination, Source),
    ADC(Destination, Source),
    SUB(Destination, Source),
    SBC(Destination, Source),
    AND(Destination, Source),
    OR(Destination, Source),
    XOR(Destination, Source),
    CP(Destination, Source),
    RLC(Destination),
}
pub struct Cpu {
    mmu: Mmu,
    // general purpose registers
    v: [u8; 16],
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    // address store register
    i: usize,

    stack: [usize; 16],
    // TODO: Make pc private
    pub pc: usize,
    sp: usize,

    // PREFIX, 0xCB
    cb: bool,
}

impl Cpu {
    pub fn new(mmu: Mmu) -> Cpu {
        Cpu {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: 0,
            h: 0,
            l: 0,
            mmu,
            v: [0u8; 16],
            i: 0,
            stack: [0usize; 16],
            pc: 0,
            sp: 0,
            cb: false,
        }
    }

    // pub fn hl(&self) -> usize {
    //     (((self.h as u16) << 8) + self.l as u16) as usize
    // }

    // pub fn write_hl(&mut self, value: u16) {
    //     self.l = (value & 0x0F) as u8;
    //     self.h = ((value & 0xF0) >> 8) as u8;
    // }

    pub fn byte(&mut self) -> Result<u8, GBError> {
        let value = self.mmu.byte(self.pc)?;
        self.pc += 1;
        Ok(value)
    }

    pub fn word(&mut self) -> Result<u16, GBError> {
        let value = self.mmu.word(self.pc)?;
        self.pc += 2;
        Ok(value)
    }

    pub fn read_instruction(&mut self) -> Result<Op, GBError> {
        let op = self.byte()?;

        if self.cb {
            self.match_cb(op)?;
            self.cb = false
        }

        match op {
            0x00 => Ok(Op::NOP),
            0x01 => {
                let value = self.word()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::BC)),
                    Source::Immediate16(value),
                ))
            }

            0x06 => {
                let value = self.byte()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::B)),
                    Source::Immediate8(value),
                ))
            }

            0x0E => {
                let value = self.byte()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::C)),
                    Source::Immediate8(value),
                ))
            }

            0x16 => {
                let value = self.byte()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::D)),
                    Source::Immediate8(value),
                ))
            }

            0x1E => {
                let value = self.byte()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::E)),
                    Source::Immediate8(value),
                ))
            }

            0x26 => {
                let value = self.byte()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::H)),
                    Source::Immediate8(value),
                ))
            }

            0x2E => {
                let value = self.byte()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::L)),
                    Source::Immediate8(value),
                ))
            }

            // LD A, x
            0x7F => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Direct(Target::Register(Register::A)),
            )),
            0x78 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Direct(Target::Register(Register::B)),
            )),
            0x79 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Direct(Target::Register(Register::C)),
            )),
            0x7A => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Direct(Target::Register(Register::D)),
            )),
            0x7B => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Direct(Target::Register(Register::E)),
            )),
            0x7C => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Direct(Target::Register(Register::H)),
            )),
            0x7D => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Direct(Target::Register(Register::L)),
            )),
            0x7E => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Indirect(Target::Register(Register::HL)),
            )),

            // LD B, x
            0x40 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::B)),
                Source::Direct(Target::Register(Register::B)),
            )),

            0x41 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::B)),
                Source::Direct(Target::Register(Register::C)),
            )),

            0x42 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::B)),
                Source::Direct(Target::Register(Register::D)),
            )),

            0x43 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::B)),
                Source::Direct(Target::Register(Register::E)),
            )),

            0x44 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::B)),
                Source::Direct(Target::Register(Register::H)),
            )),

            0x45 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::B)),
                Source::Direct(Target::Register(Register::L)),
            )),

            0x46 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::B)),
                Source::Indirect(Target::Register(Register::HL)),
            )),

            // LD C, x
            0x48 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::C)),
                Source::Direct(Target::Register(Register::B)),
            )),
            0x49 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::C)),
                Source::Direct(Target::Register(Register::C)),
            )),
            0x4A => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::C)),
                Source::Direct(Target::Register(Register::D)),
            )),
            0x4B => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::C)),
                Source::Direct(Target::Register(Register::E)),
            )),
            0x4C => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::C)),
                Source::Direct(Target::Register(Register::H)),
            )),
            0x4D => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::C)),
                Source::Direct(Target::Register(Register::L)),
            )),
            0x4E => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::C)),
                Source::Indirect(Target::Register(Register::HL)),
            )),

            // LD D, x
            0x50 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::D)),
                Source::Direct(Target::Register(Register::B)),
            )),

            0x51 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::D)),
                Source::Direct(Target::Register(Register::C)),
            )),

            0x52 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::D)),
                Source::Direct(Target::Register(Register::D)),
            )),

            0x53 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::D)),
                Source::Direct(Target::Register(Register::E)),
            )),

            0x54 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::D)),
                Source::Direct(Target::Register(Register::H)),
            )),

            0x55 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::D)),
                Source::Direct(Target::Register(Register::L)),
            )),

            0x56 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::D)),
                Source::Indirect(Target::Register(Register::HL)),
            )),

            // LD E, x
            0x58 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::E)),
                Source::Direct(Target::Register(Register::B)),
            )),
            0x59 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::E)),
                Source::Direct(Target::Register(Register::C)),
            )),
            0x5A => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::E)),
                Source::Direct(Target::Register(Register::E)),
            )),
            0x5B => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::E)),
                Source::Direct(Target::Register(Register::E)),
            )),
            0x5C => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::E)),
                Source::Direct(Target::Register(Register::H)),
            )),
            0x5D => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::E)),
                Source::Direct(Target::Register(Register::L)),
            )),
            0x5E => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::E)),
                Source::Indirect(Target::Register(Register::HL)),
            )),

            // LD H, x
            0x60 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::H)),
                Source::Direct(Target::Register(Register::B)),
            )),

            0x61 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::H)),
                Source::Direct(Target::Register(Register::C)),
            )),

            0x62 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::H)),
                Source::Direct(Target::Register(Register::D)),
            )),

            0x63 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::H)),
                Source::Direct(Target::Register(Register::E)),
            )),

            0x64 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::H)),
                Source::Direct(Target::Register(Register::H)),
            )),

            0x65 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::H)),
                Source::Direct(Target::Register(Register::L)),
            )),

            0x66 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::H)),
                Source::Indirect(Target::Register(Register::HL)),
            )),

            // LD L, x
            0x68 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::L)),
                Source::Direct(Target::Register(Register::B)),
            )),

            0x69 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::L)),
                Source::Direct(Target::Register(Register::C)),
            )),

            0x6a => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::L)),
                Source::Direct(Target::Register(Register::D)),
            )),

            0x6b => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::L)),
                Source::Direct(Target::Register(Register::E)),
            )),

            0x6c => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::L)),
                Source::Direct(Target::Register(Register::H)),
            )),

            0x6d => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::L)),
                Source::Direct(Target::Register(Register::L)),
            )),

            0x6e => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::L)),
                Source::Indirect(Target::Register(Register::HL)),
            )),

            // LD (HL), x
            0x70 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HL)),
                Source::Direct(Target::Register(Register::B)),
            )),

            0x71 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HL)),
                Source::Direct(Target::Register(Register::C)),
            )),

            0x72 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HL)),
                Source::Direct(Target::Register(Register::D)),
            )),

            0x73 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HL)),
                Source::Direct(Target::Register(Register::E)),
            )),

            0x74 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HL)),
                Source::Direct(Target::Register(Register::H)),
            )),

            0x75 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HL)),
                Source::Direct(Target::Register(Register::L)),
            )),

            // LD n, A
            0x47 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::B)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0x4f => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::C)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0x57 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::D)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0x5f => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::E)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0x67 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::H)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0x6f => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::L)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0x02 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::BC)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0x12 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::DE)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0x77 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HL)),
                Source::Direct(Target::Register(Register::A)),
            )),

            0xEA => {
                let value = self.word()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Address(value)),
                    Source::Direct(Target::Register(Register::A)),
                ))
            }

            // LD A, (C)
            // Same as LD A, (0xff00 + C)
            0xF2 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Indexed(Target::Register(Register::C), IO_REGISTER_OFFSET),
            )),

            // LD (C), A
            // Same as LD (0xff00 + C), A
            0xE2 => Ok(Op::LD(
                Destination::Indexed(Target::Register(Register::C), IO_REGISTER_OFFSET),
                Source::Direct(Target::Register(Register::A)),
            )),

            // LD A, (HL-)
            0x3A => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Indirect(Target::Register(Register::HLD)),
            )),

            // LD (HL-), A
            0x32 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HLD)),
                Source::Direct(Target::Register(Register::A)),
            )),

            // LD A, (HLI)
            0x2A => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::A)),
                Source::Indirect(Target::Register(Register::HLI)),
            )),

            // LD (HLI), A
            0x22 => Ok(Op::LD(
                Destination::Indirect(Target::Register(Register::HLI)),
                Source::Direct(Target::Register(Register::A)),
            )),

            // LD ($FF00 + n), A
            0xE0 => {
                let n = self.byte()? as u16;
                Ok(Op::LD(
                    Destination::Indexed(Target::Address(n), IO_REGISTER_OFFSET),
                    Source::Direct(Target::Register(Register::A)),
                ))
            }

            // LD A, ($FF00 + n)
            0xF0 => {
                let value = self.byte()? as u16;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::A)),
                    Source::Indexed(Target::Address(value), IO_REGISTER_OFFSET),
                ))
            }

            // LD $n, $nn
            0x11 => {
                let value = self.word()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::DE)),
                    Source::Immediate16(value),
                ))
            }

            0x21 => {
                let value = self.word()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::HL)),
                    Source::Immediate16(value),
                ))
            }

            0x31 => {
                let value = self.word()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::SP)),
                    Source::Immediate16(value),
                ))
            }

            // LD SP,HL
            0xF9 => Ok(Op::LD(
                Destination::Direct(Target::Register(Register::SP)),
                Source::Direct(Target::Register(Register::HL)),
            )),

            // LD HL, SP+n
            0xF8 => {
                let value = self.byte()? as u16;
                Ok(Op::LD(
                    Destination::Direct(Target::Register(Register::HL)),
                    Source::Indexed(Target::Register(Register::SP), value),
                ))
            }

            // LD (nn), SP
            0x08 => {
                let value = self.word()?;
                Ok(Op::LD(
                    Destination::Direct(Target::Address(value)),
                    Source::Direct(Target::Register(Register::SP)),
                ))
            }

            // ADD A, $n
            0x80..=0x87 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::ADD(
                    Destination::Direct(Target::Register(Register::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // ADC A, $n
            0x88..=0x8F => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::ADC(
                    Destination::Direct(Target::Register(Register::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // SUB A, $n
            0x90..=0x97 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::SUB(
                    Destination::Direct(Target::Register(Register::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // SBC A, $n
            0x98..=0x9F => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::SBC(
                    Destination::Direct(Target::Register(Register::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // AND A, $n
            0xA0..=0xA7 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::AND(
                    Destination::Direct(Target::Register(Register::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // XOR A, $n
            0xA8..=0xAF => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::XOR(
                    Destination::Direct(Target::Register(Register::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // OR A, $n
            0xB0..=0xB7 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::OR(
                    Destination::Direct(Target::Register(Register::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // CP A, $n
            0xB8..=0xBF => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::CP(
                    Destination::Direct(Target::Register(Register::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            0xCB => {
                self.cb = true;
                Ok(Op::PREFIX)
            }

            _ => Err(GBError::UnknownOperation(op)),
        }
    }

    fn match_cb(&mut self, op: u8) -> Result<Op, GBError> {
        match op {
            // RLC B->(HL)
            0x00..=0x07 => {
                let low = op & 0x0F;
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::RLC(reg))
            }

            _ => Err(GBError::UnknownOperation(op)),
        }
    }

    pub fn execute_instruction(&mut self, instruction: Op) {}

    pub fn clear_vram(&mut self) -> ProgramCounter {
        self.mmu.clear_vram();
        ProgramCounter::Next
    }
}

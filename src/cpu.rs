use crate::error::GBError;
use crate::mmu::Mmu;

const IO_REGISTER_OFFSET: u16 = 0xff00;

#[derive(Debug)]
pub enum ProgramCounter {
    Next,
    Jump(usize),
}

#[derive(Debug)]
pub enum Operand {
    Register(Register),
    IndirectRegister(Register),
    IndirectOffsetRegister(Register, u16),
    Byte(u8),
    Word(u16),
    OffsetWord(u16, u16),
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
    HLD, // HL Decrement (HL-)
    HLI, // HL Increment (HL+)
    SP,
}

#[derive(Debug)]
pub enum Op {
    NOP,
    LD(Operand, Operand),
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
        }
    }

    pub fn hl(&self) -> usize {
        (((self.h as u16) << 8) + self.l as u16) as usize
    }

    pub fn write_hl(&mut self, value: u16) {
        self.l = (value & 0x0F) as u8;
        self.h = ((value & 0xF0) >> 8) as u8;
    }

    pub fn read_instruction(&mut self) -> Result<Op, GBError> {
        let op = self.mmu.byte(self.pc)?;

        match op {
            0x00 => Ok(Op::NOP),
            0x01 => {
                let value = self.mmu.word(self.pc)?;
                Ok(Op::LD(
                    Operand::Register(Register::BC),
                    Operand::Word(value),
                ))
            }

            0x06 => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::B), Operand::Byte(value)))
            }

            0x0E => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::C), Operand::Byte(value)))
            }

            0x16 => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::D), Operand::Byte(value)))
            }

            0x1E => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::E), Operand::Byte(value)))
            }

            0x26 => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::H), Operand::Byte(value)))
            }

            0x2E => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::L), Operand::Byte(value)))
            }

            // LD A, x
            0x7F => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::Register(Register::A),
            )),
            0x78 => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::Register(Register::B),
            )),
            0x79 => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::Register(Register::C),
            )),
            0x7A => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::Register(Register::D),
            )),
            0x7B => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::Register(Register::E),
            )),
            0x7C => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::Register(Register::H),
            )),
            0x7D => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::Register(Register::L),
            )),
            0x7E => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::IndirectRegister(Register::HL),
            )),

            // LD B, x
            0x40 => Ok(Op::LD(
                Operand::Register(Register::B),
                Operand::Register(Register::B),
            )),

            0x41 => Ok(Op::LD(
                Operand::Register(Register::B),
                Operand::Register(Register::C),
            )),

            0x42 => Ok(Op::LD(
                Operand::Register(Register::B),
                Operand::Register(Register::D),
            )),

            0x43 => Ok(Op::LD(
                Operand::Register(Register::B),
                Operand::Register(Register::E),
            )),

            0x44 => Ok(Op::LD(
                Operand::Register(Register::B),
                Operand::Register(Register::H),
            )),

            0x45 => Ok(Op::LD(
                Operand::Register(Register::B),
                Operand::Register(Register::L),
            )),

            0x46 => Ok(Op::LD(
                Operand::Register(Register::B),
                Operand::IndirectRegister(Register::HL),
            )),

            // LD C, x
            0x48 => Ok(Op::LD(
                Operand::Register(Register::C),
                Operand::Register(Register::B),
            )),
            0x49 => Ok(Op::LD(
                Operand::Register(Register::C),
                Operand::Register(Register::C),
            )),
            0x4A => Ok(Op::LD(
                Operand::Register(Register::C),
                Operand::Register(Register::D),
            )),
            0x4B => Ok(Op::LD(
                Operand::Register(Register::C),
                Operand::Register(Register::E),
            )),
            0x4C => Ok(Op::LD(
                Operand::Register(Register::C),
                Operand::Register(Register::H),
            )),
            0x4D => Ok(Op::LD(
                Operand::Register(Register::C),
                Operand::Register(Register::L),
            )),
            0x4E => Ok(Op::LD(
                Operand::Register(Register::C),
                Operand::IndirectRegister(Register::HL),
            )),

            // LD D, x
            0x50 => Ok(Op::LD(
                Operand::Register(Register::D),
                Operand::Register(Register::B),
            )),

            0x51 => Ok(Op::LD(
                Operand::Register(Register::D),
                Operand::Register(Register::C),
            )),

            0x52 => Ok(Op::LD(
                Operand::Register(Register::D),
                Operand::Register(Register::D),
            )),

            0x53 => Ok(Op::LD(
                Operand::Register(Register::D),
                Operand::Register(Register::E),
            )),

            0x54 => Ok(Op::LD(
                Operand::Register(Register::D),
                Operand::Register(Register::H),
            )),

            0x55 => Ok(Op::LD(
                Operand::Register(Register::D),
                Operand::Register(Register::L),
            )),

            0x56 => Ok(Op::LD(
                Operand::Register(Register::D),
                Operand::IndirectRegister(Register::HL),
            )),

            // LD E, x
            0x58 => Ok(Op::LD(
                Operand::Register(Register::E),
                Operand::Register(Register::B),
            )),
            0x59 => Ok(Op::LD(
                Operand::Register(Register::E),
                Operand::Register(Register::C),
            )),
            0x5A => Ok(Op::LD(
                Operand::Register(Register::E),
                Operand::Register(Register::E),
            )),
            0x5B => Ok(Op::LD(
                Operand::Register(Register::E),
                Operand::Register(Register::E),
            )),
            0x5C => Ok(Op::LD(
                Operand::Register(Register::E),
                Operand::Register(Register::H),
            )),
            0x5D => Ok(Op::LD(
                Operand::Register(Register::E),
                Operand::Register(Register::L),
            )),
            0x5E => Ok(Op::LD(
                Operand::Register(Register::E),
                Operand::IndirectRegister(Register::HL),
            )),

            // LD H, x
            0x60 => Ok(Op::LD(
                Operand::Register(Register::H),
                Operand::Register(Register::B),
            )),

            0x61 => Ok(Op::LD(
                Operand::Register(Register::H),
                Operand::Register(Register::C),
            )),

            0x62 => Ok(Op::LD(
                Operand::Register(Register::H),
                Operand::Register(Register::D),
            )),

            0x63 => Ok(Op::LD(
                Operand::Register(Register::H),
                Operand::Register(Register::E),
            )),

            0x64 => Ok(Op::LD(
                Operand::Register(Register::H),
                Operand::Register(Register::H),
            )),

            0x65 => Ok(Op::LD(
                Operand::Register(Register::H),
                Operand::Register(Register::L),
            )),

            0x66 => Ok(Op::LD(
                Operand::Register(Register::H),
                Operand::IndirectRegister(Register::HL),
            )),

            // LD L, x
            0x68 => Ok(Op::LD(
                Operand::Register(Register::L),
                Operand::Register(Register::B),
            )),

            0x69 => Ok(Op::LD(
                Operand::Register(Register::L),
                Operand::Register(Register::C),
            )),

            0x6a => Ok(Op::LD(
                Operand::Register(Register::L),
                Operand::Register(Register::D),
            )),

            0x6b => Ok(Op::LD(
                Operand::Register(Register::L),
                Operand::Register(Register::E),
            )),

            0x6c => Ok(Op::LD(
                Operand::Register(Register::L),
                Operand::Register(Register::H),
            )),

            0x6d => Ok(Op::LD(
                Operand::Register(Register::L),
                Operand::Register(Register::L),
            )),

            0x6e => Ok(Op::LD(
                Operand::Register(Register::L),
                Operand::IndirectRegister(Register::HL),
            )),

            // LD (HL), x
            0x70 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HL),
                Operand::Register(Register::B),
            )),

            0x71 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HL),
                Operand::Register(Register::C),
            )),

            0x72 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HL),
                Operand::Register(Register::D),
            )),

            0x73 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HL),
                Operand::Register(Register::E),
            )),

            0x74 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HL),
                Operand::Register(Register::H),
            )),

            0x75 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HL),
                Operand::Register(Register::L),
            )),

            // LD n, A
            0x47 => Ok(Op::LD(
                Operand::Register(Register::B),
                Operand::Register(Register::A),
            )),

            0x4f => Ok(Op::LD(
                Operand::Register(Register::C),
                Operand::Register(Register::A),
            )),

            0x57 => Ok(Op::LD(
                Operand::Register(Register::D),
                Operand::Register(Register::A),
            )),

            0x5f => Ok(Op::LD(
                Operand::Register(Register::E),
                Operand::Register(Register::A),
            )),

            0x67 => Ok(Op::LD(
                Operand::Register(Register::H),
                Operand::Register(Register::A),
            )),

            0x6f => Ok(Op::LD(
                Operand::Register(Register::L),
                Operand::Register(Register::A),
            )),

            0x02 => Ok(Op::LD(
                Operand::IndirectRegister(Register::BC),
                Operand::Register(Register::A),
            )),

            0x12 => Ok(Op::LD(
                Operand::IndirectRegister(Register::DE),
                Operand::Register(Register::A),
            )),

            0x77 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HL),
                Operand::Register(Register::A),
            )),

            0xea => {
                let value = self.mmu.word(self.pc)?;
                Ok(Op::LD(Operand::Word(value), Operand::Register(Register::A)))
            }

            // LD A, (C)
            // Same as LD A, (0xff00 + C)
            0xf2 => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::IndirectOffsetRegister(Register::C, IO_REGISTER_OFFSET),
            )),

            // LD (C), A
            // Same as LD (0xff00 + C), A
            0xe2 => Ok(Op::LD(
                Operand::IndirectOffsetRegister(Register::C, IO_REGISTER_OFFSET),
                Operand::Register(Register::A),
            )),

            // LD A, (HLD)
            0x3a => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::IndirectRegister(Register::HLD),
            )),

            // LD (HLD), A
            0x32 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HLD),
                Operand::Register(Register::A),
            )),

            // LD A, (HLI)
            0x2a => Ok(Op::LD(
                Operand::Register(Register::A),
                Operand::IndirectRegister(Register::HLI),
            )),

            // LD (HLI), A
            0x22 => Ok(Op::LD(
                Operand::IndirectRegister(Register::HLI),
                Operand::Register(Register::A),
            )),

            // LD ($FF00 + n), A
            0xe0 => {
                let n = self.mmu.byte(self.pc)? as u16;
                Ok(Op::LD(
                    Operand::OffsetWord(n, IO_REGISTER_OFFSET),
                    Operand::Register(Register::A),
                ))
            }

            // LD A, ($FF00 + n)
            0xf0 => {
                let value = self.mmu.byte(self.pc)? as u16;
                Ok(Op::LD(
                    Operand::Register(Register::A),
                    Operand::OffsetWord(value, IO_REGISTER_OFFSET),
                ))
            }

            // LD $n, $nn
            0x11 => {
                let value = self.mmu.word(self.pc)?;
                Ok(Op::LD(
                    Operand::Register(Register::DE),
                    Operand::Word(value),
                ))
            }

            0x21 => {
                let value = self.mmu.word(self.pc)?;
                Ok(Op::LD(
                    Operand::Register(Register::HL),
                    Operand::Word(value),
                ))
            }

            0x31 => {
                let value = self.mmu.word(self.pc + 1)?;
                Ok(Op::LD(
                    Operand::Register(Register::SP),
                    Operand::Word(value),
                ))
            }

            // LD SP,HL
            0xf9 => Ok(Op::LD(
                Operand::Register(Register::SP),
                Operand::Register(Register::HL),
            )),

            // LD HL, SP+n
            0xf8 => {
                let value = self.mmu.byte(self.pc + 1)? as u16;
                Ok(Op::LD(
                    Operand::Register(Register::HL),
                    Operand::IndirectOffsetRegister(Register::SP, value),
                ))
            }

            // LD (nn), SP
            0x08 => {
                let value = self.mmu.word(self.pc + 1)?;
                Ok(Op::LD(
                    Operand::Word(value),
                    Operand::Register(Register::SP),
                ))
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

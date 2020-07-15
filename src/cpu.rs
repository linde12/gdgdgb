use crate::error::GBError;
use crate::mmu::Mmu;

#[derive(Debug)]
pub enum ProgramCounter {
    Next,
    Jump(usize),
}

#[derive(Debug)]
pub enum Operand {
    Direct(Register), // direct addressing, take the value of the register
    Indirect(Register), // indirect addressing, take the value at the address the register is pointing to
    Byte(u8),
    Word(u16),
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
                    Operand::Direct(Register::BC),
                    Operand::Word(value),
                ))
            }

            0x06 => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Direct(Register::B), Operand::Byte(value)))
            }

            0x0E => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Direct(Register::C), Operand::Byte(value)))
            }

            0x16 => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Direct(Register::D), Operand::Byte(value)))
            }

            0x1E => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Direct(Register::E), Operand::Byte(value)))
            }

            0x26 => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Direct(Register::H), Operand::Byte(value)))
            }

            0x2E => {
                let value = self.mmu.byte(self.pc)?;
                Ok(Op::LD(Operand::Direct(Register::L), Operand::Byte(value)))
            }

            // LD A, x
            0x7F => Ok(Op::LD(
                Operand::Direct(Register::A),
                Operand::Direct(Register::A),
            )),
            0x78 => Ok(Op::LD(
                Operand::Direct(Register::A),
                Operand::Direct(Register::B),
            )),
            0x79 => Ok(Op::LD(
                Operand::Direct(Register::A),
                Operand::Direct(Register::C),
            )),
            0x7A => Ok(Op::LD(
                Operand::Direct(Register::A),
                Operand::Direct(Register::D),
            )),
            0x7B => Ok(Op::LD(
                Operand::Direct(Register::A),
                Operand::Direct(Register::E),
            )),
            0x7C => Ok(Op::LD(
                Operand::Direct(Register::A),
                Operand::Direct(Register::H),
            )),
            0x7D => Ok(Op::LD(
                Operand::Direct(Register::A),
                Operand::Direct(Register::L),
            )),
            0x7E => Ok(Op::LD(
                Operand::Direct(Register::A),
                Operand::Indirect(Register::HL),
            )),

            // LD B, x
            0x40 => Ok(Op::LD(
                Operand::Direct(Register::B),
                Operand::Direct(Register::B),
            )),

            0x41 => Ok(Op::LD(
                Operand::Direct(Register::B),
                Operand::Direct(Register::C),
            )),

            0x42 => Ok(Op::LD(
                Operand::Direct(Register::B),
                Operand::Direct(Register::D),
            )),

            0x43 => Ok(Op::LD(
                Operand::Direct(Register::B),
                Operand::Direct(Register::E),
            )),

            0x44 => Ok(Op::LD(
                Operand::Direct(Register::B),
                Operand::Direct(Register::H),
            )),

            0x45 => Ok(Op::LD(
                Operand::Direct(Register::B),
                Operand::Direct(Register::L),
            )),

            0x46 => Ok(Op::LD(
                Operand::Direct(Register::B),
                Operand::Indirect(Register::HL),
            )),

            // LD C, x
            0x48 => Ok(Op::LD(
                Operand::Direct(Register::C),
                Operand::Direct(Register::B),
            )),
            0x49 => Ok(Op::LD(
                Operand::Direct(Register::C),
                Operand::Direct(Register::C),
            )),
            0x4A => Ok(Op::LD(
                Operand::Direct(Register::C),
                Operand::Direct(Register::D),
            )),
            0x4B => Ok(Op::LD(
                Operand::Direct(Register::C),
                Operand::Direct(Register::E),
            )),
            0x4C => Ok(Op::LD(
                Operand::Direct(Register::C),
                Operand::Direct(Register::H),
            )),
            0x4D => Ok(Op::LD(
                Operand::Direct(Register::C),
                Operand::Direct(Register::L),
            )),
            0x4E => Ok(Op::LD(
                Operand::Direct(Register::C),
                Operand::Indirect(Register::HL),
            )),

            // LD D, x
            0x50 => Ok(Op::LD(
                Operand::Direct(Register::D),
                Operand::Direct(Register::B),
            )),

            0x51 => Ok(Op::LD(
                Operand::Direct(Register::D),
                Operand::Direct(Register::C),
            )),

            0x52 => Ok(Op::LD(
                Operand::Direct(Register::D),
                Operand::Direct(Register::D),
            )),

            0x53 => Ok(Op::LD(
                Operand::Direct(Register::D),
                Operand::Direct(Register::E),
            )),

            0x54 => Ok(Op::LD(
                Operand::Direct(Register::D),
                Operand::Direct(Register::H),
            )),

            0x55 => Ok(Op::LD(
                Operand::Direct(Register::D),
                Operand::Direct(Register::L),
            )),

            0x56 => Ok(Op::LD(
                Operand::Direct(Register::D),
                Operand::Indirect(Register::HL),
            )),

            // LD E, x
            0x58 => Ok(Op::LD(
                Operand::Direct(Register::E),
                Operand::Direct(Register::B),
            )),
            0x59 => Ok(Op::LD(
                Operand::Direct(Register::E),
                Operand::Direct(Register::C),
            )),
            0x5A => Ok(Op::LD(
                Operand::Direct(Register::E),
                Operand::Direct(Register::E),
            )),
            0x5B => Ok(Op::LD(
                Operand::Direct(Register::E),
                Operand::Direct(Register::E),
            )),
            0x5C => Ok(Op::LD(
                Operand::Direct(Register::E),
                Operand::Direct(Register::H),
            )),
            0x5D => Ok(Op::LD(
                Operand::Direct(Register::E),
                Operand::Direct(Register::L),
            )),
            0x5E => Ok(Op::LD(
                Operand::Direct(Register::E),
                Operand::Indirect(Register::HL),
            )),

            // LD H, x
            0x60 => Ok(Op::LD(
                Operand::Direct(Register::H),
                Operand::Direct(Register::B),
            )),

            0x61 => Ok(Op::LD(
                Operand::Direct(Register::H),
                Operand::Direct(Register::C),
            )),

            0x62 => Ok(Op::LD(
                Operand::Direct(Register::H),
                Operand::Direct(Register::D),
            )),

            0x63 => Ok(Op::LD(
                Operand::Direct(Register::H),
                Operand::Direct(Register::E),
            )),

            0x64 => Ok(Op::LD(
                Operand::Direct(Register::H),
                Operand::Direct(Register::H),
            )),

            0x65 => Ok(Op::LD(
                Operand::Direct(Register::H),
                Operand::Direct(Register::L),
            )),

            0x66 => Ok(Op::LD(
                Operand::Direct(Register::H),
                Operand::Indirect(Register::HL),
            )),

            // LD L, x
            0x68 => Ok(Op::LD(
                Operand::Direct(Register::L),
                Operand::Direct(Register::B),
            )),

            0x69 => Ok(Op::LD(
                Operand::Direct(Register::L),
                Operand::Direct(Register::C),
            )),

            0x6a => Ok(Op::LD(
                Operand::Direct(Register::L),
                Operand::Direct(Register::D),
            )),

            0x6b => Ok(Op::LD(
                Operand::Direct(Register::L),
                Operand::Direct(Register::E),
            )),

            0x6c => Ok(Op::LD(
                Operand::Direct(Register::L),
                Operand::Direct(Register::H),
            )),

            0x6d => Ok(Op::LD(
                Operand::Direct(Register::L),
                Operand::Direct(Register::L),
            )),

            0x6e => Ok(Op::LD(
                Operand::Direct(Register::L),
                Operand::Indirect(Register::HL),
            )),

            // LD (HL), x
            0x70 => Ok(Op::LD(
                Operand::Indirect(Register::HL),
                Operand::Direct(Register::B),
            )),

            0x71 => Ok(Op::LD(
                Operand::Indirect(Register::HL),
                Operand::Direct(Register::C),
            )),

            0x72 => Ok(Op::LD(
                Operand::Indirect(Register::HL),
                Operand::Direct(Register::D),
            )),

            0x73 => Ok(Op::LD(
                Operand::Indirect(Register::HL),
                Operand::Direct(Register::E),
            )),

            0x74 => Ok(Op::LD(
                Operand::Indirect(Register::HL),
                Operand::Direct(Register::H),
            )),

            0x75 => Ok(Op::LD(
                Operand::Indirect(Register::HL),
                Operand::Direct(Register::L),
            )),

            // LD SP, $nn
            0x31 => {
                let value = self.mmu.word(self.pc + 1)?;
                Ok(Op::LD(
                    Operand::Direct(Register::SP),
                    Operand::Word(value),
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

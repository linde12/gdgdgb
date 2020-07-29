use crate::error::GBError;
use crate::mmu::Mmu;
use crate::register::{Flag, FlagsRegister, Register, RegisterType16, RegisterType8};
use std::fmt;

const IO_REGISTER_OFFSET: u16 = 0xff00;

// Used to algorithmically parse opcode ranges
static DESTINATION_REGISTERS: [Destination; 8] = [
    Destination::Direct(Target::Register8(RegisterType8::B)),
    Destination::Direct(Target::Register8(RegisterType8::C)),
    Destination::Direct(Target::Register8(RegisterType8::D)),
    Destination::Direct(Target::Register8(RegisterType8::E)),
    Destination::Direct(Target::Register8(RegisterType8::H)),
    Destination::Direct(Target::Register8(RegisterType8::L)),
    Destination::Indirect(RegisterType16::HL), // (HL)
    Destination::Direct(Target::Register8(RegisterType8::A)),
];

// Used to algorithmically parse opcode ranges
static SOURCE_REGISTERS: [Source; 8] = [
    Source::Direct(Target::Register8(RegisterType8::B)),
    Source::Direct(Target::Register8(RegisterType8::C)),
    Source::Direct(Target::Register8(RegisterType8::D)),
    Source::Direct(Target::Register8(RegisterType8::E)),
    Source::Direct(Target::Register8(RegisterType8::H)),
    Source::Direct(Target::Register8(RegisterType8::L)),
    Source::Indirect(RegisterType16::HL), // (HL)
    Source::Direct(Target::Register8(RegisterType8::A)),
];

// Used to algorithmically parse opcode ranges
static REGISTERS: [RegisterType; 8] = [
    RegisterType::Register8(RegisterType8::B),
    RegisterType::Register8(RegisterType8::C),
    RegisterType::Register8(RegisterType8::D),
    RegisterType::Register8(RegisterType8::E),
    RegisterType::Register8(RegisterType8::H),
    RegisterType::Register8(RegisterType8::L),
    RegisterType::Register16(RegisterType16::HL), // (HL)
    RegisterType::Register8(RegisterType8::A),
];

// See https://www.cs.helsinki.fi/u/kerola/tito/koksi_doc/memaddr.html
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Destination {
    Direct(Target),              // Direct value, either a register or u16 address
    Indirect(RegisterType16), // A pointer to an address, either from register or an address location
    Indexed(IndexedTarget, u16), // Value of target+offset, where target can be a value in a register or a u16 and offset is a u16
}

// See https://www.cs.helsinki.fi/u/kerola/tito/koksi_doc/memaddr.html
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Source {
    Immediate8(u8),
    Immediate16(u16),
    Direct(Target),              // Direct value, either a register or u16 address
    Indirect(RegisterType16), // A pointer to an address, either from register or an address location
    Indexed(IndexedTarget, u16), // Value of target+offset, where target can be a value in a register or a u16 and offset is a u16
    Offset(RegisterType16, i8),  // E.g. 0xF8 => LD HL, SP+i8
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Target {
    Register16(RegisterType16),
    Register8(RegisterType8),
    Address(usize),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IndexedTarget {
    // e.g. (HL) or ($ff00)
    Register8(RegisterType8),
    Immediate8(usize),
}

#[derive(Debug, Copy, Clone)]
pub enum Condition {
    NZ, // Not Zero
    NC, // No Carry
    Z,  // Zero
    C,  // Carry
}

impl Condition {
    fn is_satisfied(&self, flags: FlagsRegister) -> bool {
        match self {
            Condition::NZ => !flags.z,
            Condition::NC => !flags.c,
            Condition::Z => flags.z,
            Condition::C => flags.c,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum RegisterType {
    Register16(RegisterType16),
    Register8(RegisterType8),
}

#[derive(Debug, Copy, Clone)]
pub enum Op {
    NOP,
    STOP,
    RLA,
    RRA,
    RLCA,
    RRCA,
    CPL,
    CCF,
    DAA,
    SCF,
    HALT,
    DI,
    EI,
    RETI,   // Enables interrupts after return operation
    PREFIX, // 0xCB
    PUSH(RegisterType16),
    POP(RegisterType16),
    CALL(Option<Condition>, u16),
    RET(Option<Condition>),
    JR(Option<Condition>, i8),
    INC(Destination),
    DEC(Destination),
    LD(Destination, Source), // Load Operand 2 into Operand 1
    LDD(Destination, Source),
    LDI(Destination, Source),
    LDi8(Destination, Source),
    ADDi8(RegisterType16, i8),
    ADD(Source),
    ADC(Source),
    SUB(Source),
    SBC(Source),
    AND(Source),
    OR(Source),
    XOR(Source),
    CP(Source),
    RST(u8),
    RLC(Destination),
    RRC(Destination),
    RL(Destination),
    RR(Destination),
    SLA(Destination),
    SRA(Destination),
    SWAP(Destination),
    SRL(Destination),
    BIT(u8, RegisterType),
    RES(u8, RegisterType),
    SET(u8, RegisterType),
}

pub struct Cpu {
    mmu: Mmu,
    pub reg: Register,
    // PREFIX, 0xCB
    cb: bool,
}

impl Cpu {
    pub fn new(mmu: Mmu) -> Cpu {
        Cpu {
            reg: Register::new(),
            mmu,
            cb: false,
        }
    }

    pub fn pc(&self) -> usize {
        self.reg.pc
    }

    pub fn byte(&mut self) -> u8 {
        let value = self.mmu.byte(self.reg.pc);
        self.reg.pc += 1;
        value
    }

    pub fn word(&mut self) -> u16 {
        let value = self.mmu.word(self.reg.pc);
        self.reg.pc += 2;
        value
    }

    pub fn read_instruction(&mut self) -> Result<Op, GBError> {
        let op = self.byte();

        if self.cb {
            self.cb = false;
            return self.match_cb(op);
        }

        match op {
            0x00 => Ok(Op::NOP),
            0x10 => {
                self.reg.pc += 1;
                Ok(Op::STOP)
            }

            0x07 => Ok(Op::RLCA),
            0x0f => Ok(Op::RRCA),
            0x1f => Ok(Op::RRA),
            0x17 => Ok(Op::RLA),
            0x2f => Ok(Op::CPL),
            0x3f => Ok(Op::CCF),
            0x27 => Ok(Op::DAA),
            0x37 => Ok(Op::SCF),
            0xf3 => Ok(Op::DI),
            0xfb => Ok(Op::EI),

            // POP $n
            0xc1 => Ok(Op::POP(RegisterType16::BC)),
            0xd1 => Ok(Op::POP(RegisterType16::DE)),
            0xe1 => Ok(Op::POP(RegisterType16::HL)),
            0xf1 => Ok(Op::POP(RegisterType16::AF)),

            // PUSH $n
            0xc5 => Ok(Op::PUSH(RegisterType16::BC)),
            0xd5 => Ok(Op::PUSH(RegisterType16::DE)),
            0xe5 => Ok(Op::PUSH(RegisterType16::HL)),
            0xf5 => Ok(Op::PUSH(RegisterType16::AF)),

            // RET
            0xc0 => Ok(Op::RET(Some(Condition::NZ))),
            0xd0 => Ok(Op::RET(Some(Condition::NC))),
            0xc8 => Ok(Op::RET(Some(Condition::Z))),
            0xd8 => Ok(Op::RET(Some(Condition::C))),
            0xc9 => Ok(Op::RET(None)),
            0xd9 => Ok(Op::RETI),

            // RST 00h..30h
            0xc7 => Ok(Op::RST(0x00)),
            0xd7 => Ok(Op::RST(0x10)),
            0xe7 => Ok(Op::RST(0x20)),
            0xf7 => Ok(Op::RST(0x30)),

            // RST 08h..38h
            0xcf => Ok(Op::RST(0x08)),
            0xdf => Ok(Op::RST(0x18)),
            0xef => Ok(Op::RST(0x28)),
            0xff => Ok(Op::RST(0x38)),

            0x01 => {
                let value = self.word();
                Ok(Op::LD(
                    Destination::Direct(Target::Register16(RegisterType16::BC)),
                    Source::Immediate16(value),
                ))
            }

            0x06 => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::B)),
                    Source::Immediate8(value),
                ))
            }

            0x0E => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::C)),
                    Source::Immediate8(value),
                ))
            }

            0x16 => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::D)),
                    Source::Immediate8(value),
                ))
            }

            0x1E => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::E)),
                    Source::Immediate8(value),
                ))
            }

            0x26 => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::H)),
                    Source::Immediate8(value),
                ))
            }

            0x36 => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Indirect(RegisterType16::HL),
                    Source::Immediate8(value),
                ))
            }

            0x2E => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::L)),
                    Source::Immediate8(value),
                ))
            }

            // LD x, x
            0x40..=0x7f => {
                // Exception for address 0x76, which is HALT
                if op == 0x76 {
                    return Ok(Op::HALT);
                }
                // increment index in 8 step intervals, starting from 0
                let dst_index = ((op - 0x40) / 8) as usize;

                // use low nibble as source index
                let src_index = ((op & 0x0F) % 8) as usize;

                Ok(Op::LD(
                    DESTINATION_REGISTERS[dst_index],
                    SOURCE_REGISTERS[src_index],
                ))
            }

            0x02 => Ok(Op::LD(
                Destination::Indirect(RegisterType16::BC),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),

            0x12 => Ok(Op::LD(
                Destination::Indirect(RegisterType16::DE),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),

            0xEA => {
                let value = self.word() as usize;
                Ok(Op::LD(
                    Destination::Direct(Target::Address(value)),
                    Source::Direct(Target::Register8(RegisterType8::A)),
                ))
            }

            // LD A, (BC)
            0x0a => Ok(Op::LD(
                Destination::Direct(Target::Register8(RegisterType8::A)),
                Source::Indirect(RegisterType16::BC),
            )),

            // LD A, (DE)
            0x1a => Ok(Op::LD(
                Destination::Direct(Target::Register8(RegisterType8::A)),
                Source::Indirect(RegisterType16::DE),
            )),

            // LD A, (HL+) i.e LDI A, (HL)
            0x2a => Ok(Op::LDI(
                Destination::Direct(Target::Register8(RegisterType8::A)),
                Source::Indirect(RegisterType16::HL),
            )),

            // LD A, (HL-) i.e LDD A, (HL)
            0x3a => Ok(Op::LDD(
                Destination::Direct(Target::Register8(RegisterType8::A)),
                Source::Indirect(RegisterType16::HL),
            )),

            // LD A, u8
            0x3e => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // LD A, (0xff00 + C)
            0xF2 => Ok(Op::LD(
                Destination::Direct(Target::Register8(RegisterType8::A)),
                Source::Indexed(
                    IndexedTarget::Register8(RegisterType8::C),
                    IO_REGISTER_OFFSET,
                ),
            )),

            // LD (0xff00 + C), A
            0xE2 => Ok(Op::LD(
                Destination::Indexed(
                    IndexedTarget::Register8(RegisterType8::C),
                    IO_REGISTER_OFFSET,
                ),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),

            // LD (HL-), A
            0x32 => Ok(Op::LDD(
                Destination::Indirect(RegisterType16::HL),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),

            // LD (HL+), A
            0x22 => Ok(Op::LDI(
                Destination::Indirect(RegisterType16::HL),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),

            // LD ($FF00 + n), A
            0xE0 => {
                let n = self.byte() as usize;
                Ok(Op::LD(
                    Destination::Indexed(IndexedTarget::Immediate8(n), IO_REGISTER_OFFSET),
                    Source::Direct(Target::Register8(RegisterType8::A)),
                ))
            }

            // LD A, ($FF00 + n)
            0xF0 => {
                let value = self.byte() as usize;
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Indexed(IndexedTarget::Immediate8(value), IO_REGISTER_OFFSET),
                ))
            }

            // LD $n, $nn
            0x11 => {
                let value = self.word();
                Ok(Op::LD(
                    Destination::Direct(Target::Register16(RegisterType16::DE)),
                    Source::Immediate16(value),
                ))
            }

            0x21 => {
                let value = self.word();
                Ok(Op::LD(
                    Destination::Direct(Target::Register16(RegisterType16::HL)),
                    Source::Immediate16(value),
                ))
            }

            0x31 => {
                let value = self.word();
                Ok(Op::LD(
                    Destination::Direct(Target::Register16(RegisterType16::SP)),
                    Source::Immediate16(value),
                ))
            }

            // LD SP,HL
            0xF9 => Ok(Op::LD(
                Destination::Direct(Target::Register16(RegisterType16::SP)),
                Source::Direct(Target::Register16(RegisterType16::HL)),
            )),

            // LD HL, SP+n
            0xF8 => {
                let value = self.byte() as i8;
                Ok(Op::LD(
                    Destination::Direct(Target::Register16(RegisterType16::HL)),
                    Source::Offset(RegisterType16::SP, value),
                ))
            }

            // LD (nn), SP
            0x08 => {
                let value = self.word() as usize;
                Ok(Op::LD(
                    Destination::Direct(Target::Address(value)),
                    Source::Direct(Target::Register16(RegisterType16::SP)),
                ))
            }

            // ADD A, $n
            0x80..=0x87 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::ADD(SOURCE_REGISTERS[index]))
            }

            // ADC A, $n
            0x88..=0x8F => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::ADC(SOURCE_REGISTERS[index]))
            }

            // SUB A, $n
            0x90..=0x97 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::SUB(SOURCE_REGISTERS[index]))
            }

            // SBC A, $n
            0x98..=0x9F => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::SBC(SOURCE_REGISTERS[index]))
            }

            // AND HL, $nn
            0x09 | 0x19 | 0x29 | 0x39 => {
                let register_order: [Source; 4] = [
                    Source::Direct(Target::Register16(RegisterType16::BC)),
                    Source::Direct(Target::Register16(RegisterType16::DE)),
                    Source::Direct(Target::Register16(RegisterType16::HL)),
                    Source::Direct(Target::Register16(RegisterType16::SP)),
                ];

                let index = ((op & 0xF0) >> 4) as usize; // High nibble as index
                Ok(Op::AND(register_order[index]))
            }

            // AND A, $n
            0xA0..=0xA7 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::AND(SOURCE_REGISTERS[index]))
            }

            // XOR A, $n
            0xA8..=0xAF => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::XOR(SOURCE_REGISTERS[index]))
            }

            // OR A, $n
            0xB0..=0xB7 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::OR(SOURCE_REGISTERS[index]))
            }

            // CP A, $n
            0xB8..=0xBF => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::CP(SOURCE_REGISTERS[index]))
            }

            // INC $nn
            0x03 | 0x13 | 0x23 | 0x33 => {
                let register_order: [Destination; 4] = [
                    Destination::Direct(Target::Register16(RegisterType16::BC)),
                    Destination::Direct(Target::Register16(RegisterType16::DE)),
                    Destination::Direct(Target::Register16(RegisterType16::HL)),
                    Destination::Direct(Target::Register16(RegisterType16::SP)),
                ];

                let index = ((op & 0xF0) >> 4) as usize; // High nibble as index
                Ok(Op::INC(register_order[index]))
            }

            // INC $n
            0x04 | 0x14 | 0x24 | 0x34 => {
                let register_order: [Destination; 4] = [
                    Destination::Direct(Target::Register8(RegisterType8::B)),
                    Destination::Direct(Target::Register8(RegisterType8::D)),
                    Destination::Direct(Target::Register8(RegisterType8::H)),
                    Destination::Indirect(RegisterType16::HL),
                ];

                let index = ((op & 0xF0) >> 4) as usize; // High nibble as index
                Ok(Op::INC(register_order[index]))
            }

            // INC $n
            0x0C | 0x1C | 0x2C | 0x3C => {
                let register_order: [Destination; 4] = [
                    Destination::Direct(Target::Register8(RegisterType8::C)),
                    Destination::Direct(Target::Register8(RegisterType8::E)),
                    Destination::Direct(Target::Register8(RegisterType8::L)),
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                ];

                let index = ((op & 0xF0) >> 4) as usize; // High nibble as index
                Ok(Op::INC(register_order[index]))
            }

            // DEC $n
            0x0D | 0x1D | 0x2D | 0x3D => {
                let register_order: [Destination; 4] = [
                    Destination::Direct(Target::Register8(RegisterType8::C)),
                    Destination::Direct(Target::Register8(RegisterType8::E)),
                    Destination::Direct(Target::Register8(RegisterType8::L)),
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                ];

                let index = ((op & 0xF0) >> 4) as usize; // High nibble as index
                Ok(Op::DEC(register_order[index]))
            }

            // DEC $nn
            0x0b | 0x1b | 0x2b | 0x3b => {
                let register_order: [Destination; 4] = [
                    Destination::Direct(Target::Register16(RegisterType16::BC)),
                    Destination::Direct(Target::Register16(RegisterType16::DE)),
                    Destination::Direct(Target::Register16(RegisterType16::HL)),
                    Destination::Direct(Target::Register16(RegisterType16::SP)),
                ];

                let index = ((op & 0xF0) >> 4) as usize; // High nibble as index
                Ok(Op::DEC(register_order[index]))
            }

            // DEC $n
            0x05 | 0x15 | 0x25 | 0x35 => {
                let register_order: [Destination; 4] = [
                    Destination::Direct(Target::Register8(RegisterType8::B)),
                    Destination::Direct(Target::Register8(RegisterType8::D)),
                    Destination::Direct(Target::Register8(RegisterType8::H)),
                    Destination::Indirect(RegisterType16::HL),
                ];

                let index = ((op & 0xF0) >> 4) as usize; // High nibble as index
                Ok(Op::DEC(register_order[index]))
            }

            // JR, i8
            0x18 => {
                let value = self.byte() as i8;
                Ok(Op::JR(None, value))
            }

            // JR NZ, i8
            0x20 => {
                let value = self.byte() as i8;
                Ok(Op::JR(Some(Condition::NZ), value))
            }

            // JR NC, i8
            0x30 => {
                let value = self.byte() as i8;
                Ok(Op::JR(Some(Condition::NC), value))
            }

            // JR Z, i8
            0x28 => {
                let value = self.byte() as i8;
                Ok(Op::JR(Some(Condition::Z), value))
            }

            // JR C, i8
            0x38 => {
                let value = self.byte() as i8;
                Ok(Op::JR(Some(Condition::C), value))
            }

            // CALL $cond, u16
            0xc4 => {
                let value = self.word();
                Ok(Op::CALL(Some(Condition::NZ), value))
            }

            0xd4 => {
                let value = self.word();
                Ok(Op::CALL(Some(Condition::NC), value))
            }

            0xcc => {
                let value = self.word();
                Ok(Op::CALL(Some(Condition::Z), value))
            }

            0xdc => {
                let value = self.word();
                Ok(Op::CALL(Some(Condition::C), value))
            }

            0xcd => {
                let value = self.word();
                Ok(Op::CALL(None, value))
            }

            // ADC A, u8
            0xce => {
                let value = self.byte();
                Ok(Op::ADC(Source::Immediate8(value)))
            }

            // SBC A, u8
            0xde => {
                let value = self.byte();
                Ok(Op::SBC(Source::Immediate8(value)))
            }

            // XOR A, u8
            0xee => {
                let value = self.byte();
                Ok(Op::XOR(Source::Immediate8(value)))
            }

            // CP A, u8
            0xfe => {
                let value = self.byte();
                Ok(Op::CP(Source::Immediate8(value)))
            }

            // ADD A, u8
            0xc6 => {
                let value = self.byte();
                Ok(Op::ADD(Source::Immediate8(value)))
            }

            // SUB A, u8
            0xd6 => {
                let value = self.byte();
                Ok(Op::SUB(Source::Immediate8(value)))
            }

            // AND A, u8
            0xe6 => {
                let value = self.byte();
                Ok(Op::AND(Source::Immediate8(value)))
            }

            // OR A, u8
            0xf6 => {
                let value = self.byte();
                Ok(Op::OR(Source::Immediate8(value)))
            }

            // ADD SP, i8
            0xe8 => {
                let value = self.byte() as i8;
                Ok(Op::ADDi8(RegisterType16::SP, value))
            }

            // LD HL, SP+i8
            0xfa => {
                let value = self.byte() as i8;
                Ok(Op::LDi8(
                    Destination::Direct(Target::Register16(RegisterType16::HL)),
                    Source::Offset(RegisterType16::SP, value),
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
            // RLC B->A
            0x00..=0x07 => {
                let low = op & 0x0F;
                let reg = DESTINATION_REGISTERS[low as usize];
                Ok(Op::RLC(reg))
            }

            // RRC B->A
            0x08..=0x0F => {
                let low = op & 0x0F;
                let reg = DESTINATION_REGISTERS[low as usize];
                Ok(Op::RRC(reg))
            }

            // RL B->A
            0x10..=0x17 => {
                let low = op & 0x0F;
                let reg = DESTINATION_REGISTERS[low as usize];
                Ok(Op::RL(reg))
            }

            // RR B->A
            0x18..=0x1F => {
                let low = op & 0x0F;
                let reg = DESTINATION_REGISTERS[low as usize];
                Ok(Op::RR(reg))
            }

            // SLA B->A
            0x20..=0x27 => {
                let low = op & 0x0F;
                let reg = DESTINATION_REGISTERS[low as usize];
                Ok(Op::SLA(reg))
            }

            // SLR B->A
            0x28..=0x2F => {
                let low = op & 0x0F;
                let reg = DESTINATION_REGISTERS[low as usize];
                Ok(Op::SRA(reg))
            }

            // SWAP B->A
            0x30..=0x37 => {
                let low = op & 0x0F;
                let reg = DESTINATION_REGISTERS[low as usize];
                Ok(Op::SWAP(reg))
            }

            // SRL B->A
            0x38..=0x3F => {
                let low = op & 0x0F;
                let reg = DESTINATION_REGISTERS[low as usize];
                Ok(Op::SRL(reg))
            }

            // BIT index, $n
            0x40..=0x7f => {
                // increment index in 8 step intervals, starting from 0
                let dst_index = ((op - 0x40) / 8) as u8;

                // use low nibble as source index
                let src_index = ((op & 0x0F) % 8) as usize;

                Ok(Op::BIT(dst_index, REGISTERS[src_index]))
            }

            // RES index, $n
            0x80..=0xbf => {
                // increment index in 8 step intervals, starting from 0
                let dst_index = ((op - 0x80) / 8) as u8;

                // use low nibble as source index
                let src_index = ((op & 0x0F) % 8) as usize;

                Ok(Op::RES(dst_index, REGISTERS[src_index]))
            }

            // SET index, $n
            0xc0..=0xff => {
                // increment index in 8 step intervals, starting from 0
                let dst_index = ((op - 0xc0) / 8) as u8;

                // use low nibble as source index
                let src_index = ((op & 0x0F) % 8) as usize;

                Ok(Op::SET(dst_index, REGISTERS[src_index]))
            }
        }
    }

    pub fn execute_instruction(&mut self, instruction: Op) {
        match instruction {
            Op::JR(flag, offset) => self.jr(flag, offset),
            Op::BIT(n, src) => self.bit(n, src),
            Op::LD(dst, src) => self.ld(dst, src),
            Op::LDD(dst, src) => self.ldd(dst, src),
            Op::LDI(dst, src) => self.ldi(dst, src),
            // Op::NOP => {}
            // Op::STOP => {}
            Op::RLA => {
                let ticks = self.rl(Destination::Direct(Target::Register8(RegisterType8::A)));
                self.reg.set_flag(Flag::Zero, false);
                ticks
            }
            // Op::RRA => {}
            // Op::RLCA => {}
            // Op::RRCA => {}
            // Op::CPL => {}
            // Op::CCF => {}
            // Op::DAA => {}
            // Op::SCF => {}
            // Op::HALT => {}
            // Op::DI => {}
            // Op::EI => {}
            // Op::RETI => {}
            Op::PREFIX => 0,
            Op::PUSH(reg) => self.push(reg),
            Op::POP(reg) => self.pop(reg),
            Op::CALL(condition, addr) => self.call(condition, addr),
            Op::RET(cond) => self.ret(cond),
            Op::INC(dst) => self.inc(dst),
            Op::DEC(dst) => self.dec(dst),
            // Op::LDi8(_, _) => {}
            // Op::ADD(_, _) => {}
            // Op::ADDi8(_, _) => {}
            // Op::ADC(_, _) => {}
            // Op::SUB(_, _) => {}
            Op::SBC(src) => self.sbc(src, true),
            // Op::AND(_, _) => {}
            // Op::OR(_, _) => {}
            Op::XOR(src) => self.xor(src),
            // Op::CP(_, _) => {}
            // Op::RST(_) => {}
            // Op::RLC(_) => {}
            // Op::RRC(_) => {}
            Op::RL(dst) => self.rl(dst),
            // Op::RR(_) => {}
            // Op::SLA(_) => {}
            // Op::SRA(_) => {}
            // Op::SWAP(_) => {}
            // Op::SRL(_) => {}
            // Op::RES(_, _) => {}
            // Op::SET(_, _) => {}
            instr => unimplemented!("instruction {:?} not implemented", instr),
        };
    }

    fn jr(&mut self, flag: Option<Condition>, offset: i8) -> u8 {
        let op_pc = self.reg.pc;
        let addr = op_pc.wrapping_add(offset as usize);
        if let Some(flag) = flag {
            let cpu_flags: FlagsRegister = self.reg.f.into();
            match flag {
                Condition::NZ => {
                    if !cpu_flags.z {
                        self.reg.pc = addr;
                    }
                }
                Condition::Z => {
                    if cpu_flags.z {
                        self.reg.pc = addr;
                    }
                }
                Condition::NC => {
                    if !cpu_flags.c {
                        self.reg.pc = addr;
                    }
                }
                Condition::C => {
                    if cpu_flags.c {
                        self.reg.pc = addr;
                    }
                }
            };
            12
        } else {
            self.reg.pc = addr;
            8
        }
    }

    fn bit(&mut self, n: u8, source: RegisterType) -> u8 {
        let value = match source {
            RegisterType::Register8(reg) => self.reg.reg8(reg),
            // BIT n, (HL)
            RegisterType::Register16(RegisterType16::HL) => {
                let addr = self.reg.hl();
                self.mmu.byte(addr as usize)
            }
            _ => panic!("invalid BIT target"),
        };

        let is_bit_zero = value & (1 << n) == 0;
        if is_bit_zero {
            self.reg.set_flag(Flag::Zero, true);
        }
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, true);

        8
    }

    fn ld(&mut self, dst: Destination, src: Source) -> u8 {
        let src_value = self.value_from_source(src);
        self.write_into(dst, src_value);
        8
    }

    fn ldd(&mut self, dst: Destination, src: Source) -> u8 {
        let src_value = self.value_from_source(src);
        self.write_into(dst, src_value);
        self.reg.dec_hl();
        8
    }

    fn ldi(&mut self, dst: Destination, src: Source) -> u8 {
        let src_value = self.value_from_source(src);
        self.write_into(dst, src_value);
        self.reg.inc_hl();
        8
    }

    fn call(&mut self, cond: Option<Condition>, addr: u16) -> u8 {
        if let Some(condition) = cond {
            let cpu_flags: FlagsRegister = self.reg.f.into();
            if condition.is_satisfied(cpu_flags) {
                self.push_stack(self.reg.pc as u16);
                self.reg.pc = addr as usize;
                return 24;
            }
            12
        } else {
            self.push_stack(self.reg.pc as u16);
            self.reg.pc = addr as usize;
            24
        }
    }

    fn xor(&mut self, src: Source) -> u8 {
        let src_value = self.value_from_source(src) as u8;

        self.reg.a = src_value ^ self.reg.a;
        if self.reg.a == 0 {
            self.reg.set_flag(Flag::Zero, false);
        }

        8
    }

    fn sbc(&mut self, src: Source, use_carry: bool) -> u8 {
        let src_value = self.value_from_source(src) as u8;
        let carry = if use_carry && self.reg.flag(Flag::Carry) {
            1
        } else {
            0
        };

        let a = self.reg.a;
        let result = a.wrapping_sub(src_value).wrapping_sub(carry);

        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, true);
        self.reg
            .set_flag(Flag::HalfCarry, (a & 0x0F) + (src_value & 0x0F) > 0x0F);
        self.reg
            .set_flag(Flag::Carry, (a as u16) < (src_value as u16) + carry as u16);
        self.reg.a = result;

        8
    }

    fn inc(&mut self, dst: Destination) -> u8 {
        let value = self.value_from_destination(dst) as u8;
        let result = value.wrapping_add(1);
        self.reg.set_flag(Flag::Zero, result == 0);
        // if lower nibble (e.g. 1111) + 1 is greater than 0x0F(1111), the upper nibble will be
        // affected (e.g. 0001 0000) and we set half carry
        self.reg
            .set_flag(Flag::HalfCarry, (value & 0x0F) + 1 > 0x0F);
        self.reg.set_flag(Flag::Negative, false);
        self.write_into(dst, result as u16);

        8
    }

    fn dec(&mut self, dst: Destination) -> u8 {
        let value = self.value_from_destination(dst) as u8;
        let result = value.wrapping_sub(1);
        self.reg.set_flag(Flag::Zero, result == 0);
        // if lower nibble is 0000 and we are trying to subtract one, then we have to borrow from
        // upper nibble. So we set HalfCarry. E.g.
        // 30h - 1h -> 0011 0000
        //           - 0000 0001
        //             0010 1111 -> 2F (upper nibble affected)
        self.reg.set_flag(Flag::HalfCarry, (value & 0x0F) == 0);
        self.reg.set_flag(Flag::Negative, true);
        self.write_into(dst, result as u16);

        8
    }

    fn push(&mut self, reg: RegisterType16) -> u8 {
        let value = self.reg.reg16(reg);
        self.push_stack(value as u16)
    }

    fn push_stack(&mut self, word: u16) -> u8 {
        self.reg.sp -= 2;
        self.mmu.write_word(self.reg.sp, word);

        8
    }

    fn pop(&mut self, reg: RegisterType16) -> u8 {
        let value = self.mmu.word(self.reg.sp);
        self.reg.sp += 2;
        self.reg.set_reg16(reg, value);

        8
    }

    fn ret(&mut self, cond: Option<Condition>) -> u8 {
        let cpu_flags: FlagsRegister = self.reg.f.into();
        let ok = match cond {
            Some(cond) => cond.is_satisfied(cpu_flags),
            None => true,
        };

        if ok {
            let value = self.mmu.word(self.reg.sp) as usize;
            self.reg.sp += 2;
            self.reg.pc = value;
        }

        8
    }

    fn rl(&mut self, dst: Destination) -> u8 {
        let value = self.value_from_destination(dst) as u8;
        let will_carry = value & 0x80 == 0x80; // is the MSB 1? then we will carry
        let result = (value << 1) | if self.reg.flag(Flag::Carry) { 1 } else { 0 };

        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
        self.reg.set_flag(Flag::Carry, will_carry);
        self.write_into(dst, result as u16);

        8
    }

    fn write_into(&mut self, dst: Destination, src_value: u16) {
        match dst {
            Destination::Direct(target) => match target {
                Target::Register8(reg) => self.reg.set_reg8(reg, src_value as u8),
                Target::Register16(reg) => self.reg.set_reg16(reg, src_value),
                Target::Address(addr) => self.mmu.write_byte(addr, src_value as u8),
            },
            Destination::Indirect(reg) => {
                let addr = self.reg.reg16(reg);
                self.mmu.write_word(addr as usize, src_value);
            }
            Destination::Indexed(target, addr) => match target {
                IndexedTarget::Register8(reg) => {
                    let addr_offset = self.reg.reg8(reg);
                    self.mmu
                        .write_byte(addr as usize + addr_offset as usize, src_value as u8);
                }
                IndexedTarget::Immediate8(addr_offset) => {
                    self.mmu
                        .write_byte(addr as usize + addr_offset as usize, src_value as u8);
                }
            },
        }
    }

    fn value_from_destination(&self, dst: Destination) -> u16 {
        match dst {
            Destination::Direct(target) => self.direct_from_target(target),
            Destination::Indirect(target) => self.indirect_from_target(target) as u16,
            Destination::Indexed(target, offset) => self.indexed_from_target(target, offset) as u16,
        }
    }

    fn value_from_source(&self, src: Source) -> u16 {
        match src {
            Source::Immediate8(n) => n as u16,
            Source::Immediate16(nn) => nn,
            Source::Direct(target) => self.direct_from_target(target),
            Source::Indirect(target) => self.indirect_from_target(target) as u16,
            Source::Indexed(target, offset) => self.indexed_from_target(target, offset) as u16,
            Source::Offset(reg, offset) => {
                let v = self.reg.reg16(reg);
                v.wrapping_add(offset as u16)
            }
        }
    }

    fn direct_from_target(&self, target: Target) -> u16 {
        match target {
            Target::Register8(reg) => self.reg.reg8(reg) as u16,
            Target::Register16(reg) => self.reg.reg16(reg),
            Target::Address(addr) => addr as u16,
        }
    }

    fn indirect_from_target(&self, reg: RegisterType16) -> u8 {
        let addr = self.reg.reg16(reg);
        self.mmu.byte(addr as usize)
    }

    fn indexed_from_target(&self, target: IndexedTarget, offset_addr: u16) -> u8 {
        match target {
            IndexedTarget::Register8(reg) => {
                let offset = self.reg.reg8(reg);
                self.mmu.byte(offset as usize + offset_addr as usize)
            }
            IndexedTarget::Immediate8(n) => self.mmu.byte(n + offset_addr as usize),
        }
    }
}

impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            concat!(
                "PC: 0x{:04X}\n",
                "SP: 0x{:04X}\n",
                "AF: 0x{:04X}\n",
                "BC: 0x{:04X}\n",
                "DE: 0x{:04X}\n",
                "HL: 0x{:04X}\n",
                "------------\n",
                "Z: {}\n",
                "N: {}\n",
                "H: {}\n",
                "C: {}\n",
                "F: {:08b}",
            ),
            self.reg.pc,
            self.reg.sp,
            self.reg.af(),
            self.reg.bc(),
            self.reg.de(),
            self.reg.hl(),
            self.reg.flag(Flag::Zero),
            self.reg.flag(Flag::Negative),
            self.reg.flag(Flag::HalfCarry),
            self.reg.flag(Flag::Carry),
            self.reg.f,
        )
    }
}

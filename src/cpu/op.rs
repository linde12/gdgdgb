use crate::mmu::Mmu;

use super::register::FlagsRegister;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget, LoadWordSource),
    AFromIndirect(Indirect),
    IndirectFromA(Indirect),
    AFromIndirectFF00u8(u8),
    IndirectFF00u8FromA(u8),
    SPFromHL,
    HLFromSPu8(u8),
    IndirectFromSP(u16),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Indirect {
    BC,
    DE,
    HL,
    HLD,
    HLI,
    Word(u16),
    FF00PlusC,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadByteTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLIndirect,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadByteSource {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8(u8),
    HLIndirect,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadWordTarget {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadWordSource {
    D16(u16),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArithmeticTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8(u8),
    HLIndirect,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrefixTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLIndirect,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IncDecTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HL,
    BC,
    DE,
    HLIndirect,
    SP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StackTarget {
    AF,
    BC,
    DE,
    HL,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ADDHLTarget {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Condition {
    None, // No condition
    NZ,   // Not Zero
    NC,   // No Carry
    Z,    // Zero
    C,    // Carry
}

// TODO: Impl Rust traits From or Into instead?
impl Condition {
    pub fn is_satisfied(&self, flags: FlagsRegister) -> bool {
        match self {
            Condition::NZ => !flags.z,
            Condition::NC => !flags.c,
            Condition::Z => flags.z,
            Condition::C => flags.c,
            Condition::None => true,
        }
    }
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
    RETI, // Enables interrupts after return operation
    PUSH(StackTarget),
    POP(StackTarget),
    CALL(Condition, u16),
    RET(Condition),
    JR(Condition, u8),
    JP(Condition, u16),
    JPHLIndirect,
    INC(IncDecTarget),
    DEC(IncDecTarget),
    LD(LoadType), // Load Operand 2 into Operand 1
    ADD(ArithmeticTarget),
    ADDHL(ADDHLTarget),
    ADDSPi8,
    ADC(ArithmeticTarget),
    SUB(ArithmeticTarget),
    SBC(ArithmeticTarget),
    AND(ArithmeticTarget),
    OR(ArithmeticTarget),
    XOR(ArithmeticTarget),
    CP(ArithmeticTarget),
    RST(u8),

    // Prefixed ops
    RLC(PrefixTarget),
    RRC(PrefixTarget),
    RL(PrefixTarget),
    RR(PrefixTarget),
    SLA(PrefixTarget),
    SRA(PrefixTarget),
    SWAP(PrefixTarget),
    SRL(PrefixTarget),
    BIT(PrefixTarget, u8),
    RES(PrefixTarget, u8),
    SET(PrefixTarget, u8),
}

impl Op {
    pub fn read_op(mmu: &mut Mmu, mut pc: u16) -> Option<(Op, u16)> {
        let byte = mmu.byte(pc);

        let op = match byte {
            // PREFIX 0xCB
            0xCB => {
                let op = Op::op_from_prefix(mmu, pc + 1);
                pc += 1; // all ops in CB table are 1 byte
                op
            }
            // Generic
            0x00 => Some(Op::NOP),
            0x76 => Some(Op::HALT),
            0xF3 => Some(Op::DI),
            0xFB => Some(Op::EI),
            0x10 => Some(Op::STOP),
            0x07 => Some(Op::RLCA),
            0x0F => Some(Op::RRCA),
            0x1F => Some(Op::RRA),
            0x17 => Some(Op::RLA),
            0x2F => Some(Op::CPL),
            0x3F => Some(Op::CCF),
            0x27 => Some(Op::DAA),
            0x37 => Some(Op::SCF),

            0xC1 => Some(Op::POP(StackTarget::BC)),
            0xD1 => Some(Op::POP(StackTarget::DE)),
            0xE1 => Some(Op::POP(StackTarget::HL)),
            0xF1 => Some(Op::POP(StackTarget::AF)),

            0xC5 => Some(Op::PUSH(StackTarget::BC)),
            0xD5 => Some(Op::PUSH(StackTarget::DE)),
            0xE5 => Some(Op::PUSH(StackTarget::HL)),
            0xF5 => Some(Op::PUSH(StackTarget::AF)),

            0xC0 => Some(Op::RET(Condition::NZ)),
            0xD0 => Some(Op::RET(Condition::NC)),
            0xC8 => Some(Op::RET(Condition::Z)),
            0xD8 => Some(Op::RET(Condition::C)),
            0xC9 => Some(Op::RET(Condition::None)),

            0xD9 => Some(Op::RETI),

            0xC7 => Some(Op::RST(0x00)),
            0xD7 => Some(Op::RST(0x10)),
            0xE7 => Some(Op::RST(0x20)),
            0xF7 => Some(Op::RST(0x30)),
            0xCF => Some(Op::RST(0x08)),
            0xDF => Some(Op::RST(0x18)),
            0xEF => Some(Op::RST(0x28)),
            0xFF => Some(Op::RST(0x38)),

            0x01 => {
                let op = Op::LD(LoadType::Word(
                    LoadWordTarget::BC,
                    LoadWordSource::D16(mmu.word(pc + 1)),
                ));
                pc += 2;
                Some(op)
            }
            0x11 => {
                let op = Op::LD(LoadType::Word(
                    LoadWordTarget::DE,
                    LoadWordSource::D16(mmu.word(pc + 1)),
                ));
                pc += 2;
                Some(op)
            }
            0x21 => {
                let op = Op::LD(LoadType::Word(
                    LoadWordTarget::HL,
                    LoadWordSource::D16(mmu.word(pc + 1)),
                ));
                pc += 2;
                Some(op)
            }
            0x31 => {
                let op = Op::LD(LoadType::Word(
                    LoadWordTarget::SP,
                    LoadWordSource::D16(mmu.word(pc + 1)),
                ));
                pc += 2;
                Some(op)
            }

            0x40 => Some(Op::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::B))),
            0x41 => Some(Op::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::C))),
            0x42 => Some(Op::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::D))),
            0x43 => Some(Op::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::E))),
            0x44 => Some(Op::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::H))),
            0x45 => Some(Op::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::L))),
            0x46 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::HLIndirect,
            ))),
            0x47 => Some(Op::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::A))),

            0x48 => Some(Op::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::B))),
            0x49 => Some(Op::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::C))),
            0x4A => Some(Op::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::D))),
            0x4B => Some(Op::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::E))),
            0x4C => Some(Op::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::H))),
            0x4D => Some(Op::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::L))),
            0x4E => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::HLIndirect,
            ))),
            0x4F => Some(Op::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::A))),

            0x50 => Some(Op::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::B))),
            0x51 => Some(Op::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::C))),
            0x52 => Some(Op::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::D))),
            0x53 => Some(Op::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::E))),
            0x54 => Some(Op::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::H))),
            0x55 => Some(Op::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::L))),
            0x56 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::HLIndirect,
            ))),
            0x57 => Some(Op::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::A))),

            0x58 => Some(Op::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::B))),
            0x59 => Some(Op::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::C))),
            0x5A => Some(Op::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::D))),
            0x5B => Some(Op::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::E))),
            0x5C => Some(Op::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::H))),
            0x5D => Some(Op::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::L))),
            0x5E => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::HLIndirect,
            ))),
            0x5F => Some(Op::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::A))),

            0x60 => Some(Op::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::B))),
            0x61 => Some(Op::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::C))),
            0x62 => Some(Op::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::D))),
            0x63 => Some(Op::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::E))),
            0x64 => Some(Op::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::H))),
            0x65 => Some(Op::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::L))),
            0x66 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::HLIndirect,
            ))),
            0x67 => Some(Op::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::A))),

            0x68 => Some(Op::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::B))),
            0x69 => Some(Op::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::C))),
            0x6A => Some(Op::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::D))),
            0x6B => Some(Op::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::E))),
            0x6C => Some(Op::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::H))),
            0x6D => Some(Op::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::L))),
            0x6E => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::HLIndirect,
            ))),
            0x6F => Some(Op::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::A))),

            0x70 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::HLIndirect,
                LoadByteSource::B,
            ))),
            0x71 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::HLIndirect,
                LoadByteSource::C,
            ))),
            0x72 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::HLIndirect,
                LoadByteSource::D,
            ))),
            0x73 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::HLIndirect,
                LoadByteSource::E,
            ))),
            0x74 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::HLIndirect,
                LoadByteSource::H,
            ))),
            0x75 => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::HLIndirect,
                LoadByteSource::L,
            ))),
            0x77 => Some(Op::LD(LoadType::IndirectFromA(Indirect::HL))),

            0x78 => Some(Op::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::B))),
            0x79 => Some(Op::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::C))),
            0x7A => Some(Op::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::D))),
            0x7B => Some(Op::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::E))),
            0x7C => Some(Op::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::H))),
            0x7D => Some(Op::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::L))),
            0x7E => Some(Op::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::HLIndirect,
            ))),
            0x7F => Some(Op::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::A))),

            0x3E => {
                let op = Op::LD(LoadType::Byte(
                    LoadByteTarget::A,
                    LoadByteSource::D8(mmu.byte(pc + 1)),
                ));
                pc += 1;
                Some(op)
            }
            0x06 => {
                let op = Op::LD(LoadType::Byte(
                    LoadByteTarget::B,
                    LoadByteSource::D8(mmu.byte(pc + 1)),
                ));
                pc += 1;
                Some(op)
            }
            0x0E => {
                let op = Op::LD(LoadType::Byte(
                    LoadByteTarget::C,
                    LoadByteSource::D8(mmu.byte(pc + 1)),
                ));
                pc += 1;
                Some(op)
            }
            0x16 => {
                let op = Op::LD(LoadType::Byte(
                    LoadByteTarget::D,
                    LoadByteSource::D8(mmu.byte(pc + 1)),
                ));
                pc += 1;
                Some(op)
            }
            0x1E => {
                let op = Op::LD(LoadType::Byte(
                    LoadByteTarget::E,
                    LoadByteSource::D8(mmu.byte(pc + 1)),
                ));
                pc += 1;
                Some(op)
            }
            0x26 => {
                let op = Op::LD(LoadType::Byte(
                    LoadByteTarget::H,
                    LoadByteSource::D8(mmu.byte(pc + 1)),
                ));
                pc += 1;
                Some(op)
            }
            0x2E => {
                let op = Op::LD(LoadType::Byte(
                    LoadByteTarget::L,
                    LoadByteSource::D8(mmu.byte(pc + 1)),
                ));
                pc += 1;
                Some(op)
            }
            0x36 => {
                let op = Op::LD(LoadType::Byte(
                    LoadByteTarget::HLIndirect,
                    LoadByteSource::D8(mmu.byte(pc + 1)),
                ));
                pc += 1;
                Some(op)
            }

            0xF2 => Some(Op::LD(LoadType::AFromIndirect(Indirect::FF00PlusC))),
            0x0A => Some(Op::LD(LoadType::AFromIndirect(Indirect::BC))),
            0x1A => Some(Op::LD(LoadType::AFromIndirect(Indirect::DE))),
            0x2A => Some(Op::LD(LoadType::AFromIndirect(Indirect::HLI))),
            0x3A => Some(Op::LD(LoadType::AFromIndirect(Indirect::HLD))),
            0xFA => {
                let op = Op::LD(LoadType::AFromIndirect(Indirect::Word(mmu.word(pc + 1))));
                pc += 2;
                Some(op)
            }

            0xE2 => Some(Op::LD(LoadType::IndirectFromA(Indirect::FF00PlusC))),
            0x02 => Some(Op::LD(LoadType::IndirectFromA(Indirect::BC))),
            0x12 => Some(Op::LD(LoadType::IndirectFromA(Indirect::DE))),
            0x22 => Some(Op::LD(LoadType::IndirectFromA(Indirect::HLI))),
            0x32 => Some(Op::LD(LoadType::IndirectFromA(Indirect::HLD))),
            0xEA => {
                let op = Op::LD(LoadType::IndirectFromA(Indirect::Word(mmu.word(pc + 1))));
                pc += 2;
                Some(op)
            }

            0xE0 => {
                let op = Op::LD(LoadType::IndirectFF00u8FromA(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }
            0xF0 => {
                let op = Op::LD(LoadType::AFromIndirectFF00u8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0x08 => {
                let op = Op::LD(LoadType::IndirectFromSP(mmu.word(pc + 1)));
                pc += 2;
                Some(op)
            }
            0xF9 => Some(Op::LD(LoadType::SPFromHL)),
            0xF8 => {
                let op = Op::LD(LoadType::HLFromSPu8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0x87 => Some(Op::ADD(ArithmeticTarget::A)),
            0x80 => Some(Op::ADD(ArithmeticTarget::B)),
            0x81 => Some(Op::ADD(ArithmeticTarget::C)),
            0x82 => Some(Op::ADD(ArithmeticTarget::D)),
            0x83 => Some(Op::ADD(ArithmeticTarget::E)),
            0x84 => Some(Op::ADD(ArithmeticTarget::H)),
            0x85 => Some(Op::ADD(ArithmeticTarget::L)),
            0x86 => Some(Op::ADD(ArithmeticTarget::HLIndirect)),
            0xC6 => {
                let op = Op::ADD(ArithmeticTarget::D8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0x8F => Some(Op::ADC(ArithmeticTarget::A)),
            0x88 => Some(Op::ADC(ArithmeticTarget::B)),
            0x89 => Some(Op::ADC(ArithmeticTarget::C)),
            0x8A => Some(Op::ADC(ArithmeticTarget::D)),
            0x8B => Some(Op::ADC(ArithmeticTarget::E)),
            0x8C => Some(Op::ADC(ArithmeticTarget::H)),
            0x8D => Some(Op::ADC(ArithmeticTarget::L)),
            0x8E => Some(Op::ADC(ArithmeticTarget::HLIndirect)),
            0xCE => {
                let op = Op::ADC(ArithmeticTarget::D8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0x97 => Some(Op::SUB(ArithmeticTarget::A)),
            0x90 => Some(Op::SUB(ArithmeticTarget::B)),
            0x91 => Some(Op::SUB(ArithmeticTarget::C)),
            0x92 => Some(Op::SUB(ArithmeticTarget::D)),
            0x93 => Some(Op::SUB(ArithmeticTarget::E)),
            0x94 => Some(Op::SUB(ArithmeticTarget::H)),
            0x95 => Some(Op::SUB(ArithmeticTarget::L)),
            0x96 => Some(Op::SUB(ArithmeticTarget::HLIndirect)),
            0xD6 => {
                let op = Op::SUB(ArithmeticTarget::D8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0x9F => Some(Op::SBC(ArithmeticTarget::A)),
            0x98 => Some(Op::SBC(ArithmeticTarget::B)),
            0x99 => Some(Op::SBC(ArithmeticTarget::C)),
            0x9A => Some(Op::SBC(ArithmeticTarget::D)),
            0x9B => Some(Op::SBC(ArithmeticTarget::E)),
            0x9C => Some(Op::SBC(ArithmeticTarget::H)),
            0x9D => Some(Op::SBC(ArithmeticTarget::L)),
            0x9E => Some(Op::SBC(ArithmeticTarget::HLIndirect)),
            0xDE => {
                let op = Op::SBC(ArithmeticTarget::D8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0xA7 => Some(Op::AND(ArithmeticTarget::A)),
            0xA0 => Some(Op::AND(ArithmeticTarget::B)),
            0xA1 => Some(Op::AND(ArithmeticTarget::C)),
            0xA2 => Some(Op::AND(ArithmeticTarget::D)),
            0xA3 => Some(Op::AND(ArithmeticTarget::E)),
            0xA4 => Some(Op::AND(ArithmeticTarget::H)),
            0xA5 => Some(Op::AND(ArithmeticTarget::L)),
            0xA6 => Some(Op::AND(ArithmeticTarget::HLIndirect)),
            0xE6 => {
                let op = Op::AND(ArithmeticTarget::D8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0xAF => Some(Op::XOR(ArithmeticTarget::A)),
            0xA8 => Some(Op::XOR(ArithmeticTarget::B)),
            0xA9 => Some(Op::XOR(ArithmeticTarget::C)),
            0xAA => Some(Op::XOR(ArithmeticTarget::D)),
            0xAB => Some(Op::XOR(ArithmeticTarget::E)),
            0xAC => Some(Op::XOR(ArithmeticTarget::H)),
            0xAD => Some(Op::XOR(ArithmeticTarget::L)),
            0xAE => Some(Op::XOR(ArithmeticTarget::HLIndirect)),
            0xEE => {
                let op = Op::XOR(ArithmeticTarget::D8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0xB7 => Some(Op::OR(ArithmeticTarget::A)),
            0xB0 => Some(Op::OR(ArithmeticTarget::B)),
            0xB1 => Some(Op::OR(ArithmeticTarget::C)),
            0xB2 => Some(Op::OR(ArithmeticTarget::D)),
            0xB3 => Some(Op::OR(ArithmeticTarget::E)),
            0xB4 => Some(Op::OR(ArithmeticTarget::H)),
            0xB5 => Some(Op::OR(ArithmeticTarget::L)),
            0xB6 => Some(Op::OR(ArithmeticTarget::HLIndirect)),
            0xF6 => {
                let op = Op::OR(ArithmeticTarget::D8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0xBF => Some(Op::CP(ArithmeticTarget::A)),
            0xB8 => Some(Op::CP(ArithmeticTarget::B)),
            0xB9 => Some(Op::CP(ArithmeticTarget::C)),
            0xBA => Some(Op::CP(ArithmeticTarget::D)),
            0xBB => Some(Op::CP(ArithmeticTarget::E)),
            0xBC => Some(Op::CP(ArithmeticTarget::H)),
            0xBD => Some(Op::CP(ArithmeticTarget::L)),
            0xBE => Some(Op::CP(ArithmeticTarget::HLIndirect)),
            0xFE => {
                let op = Op::CP(ArithmeticTarget::D8(mmu.byte(pc + 1)));
                pc += 1;
                Some(op)
            }

            0x09 => Some(Op::ADDHL(ADDHLTarget::BC)),
            0x19 => Some(Op::ADDHL(ADDHLTarget::DE)),
            0x29 => Some(Op::ADDHL(ADDHLTarget::HL)),
            0x39 => Some(Op::ADDHL(ADDHLTarget::SP)),

            0x3C => Some(Op::INC(IncDecTarget::A)),
            0x04 => Some(Op::INC(IncDecTarget::B)),
            0x14 => Some(Op::INC(IncDecTarget::D)),
            0x24 => Some(Op::INC(IncDecTarget::H)),
            0x0C => Some(Op::INC(IncDecTarget::C)),
            0x1C => Some(Op::INC(IncDecTarget::E)),
            0x2C => Some(Op::INC(IncDecTarget::L)),
            0x34 => Some(Op::INC(IncDecTarget::HLIndirect)),
            0x03 => Some(Op::INC(IncDecTarget::BC)),
            0x13 => Some(Op::INC(IncDecTarget::DE)),
            0x23 => Some(Op::INC(IncDecTarget::HL)),
            0x33 => Some(Op::INC(IncDecTarget::SP)),

            0x3D => Some(Op::DEC(IncDecTarget::A)),
            0x05 => Some(Op::DEC(IncDecTarget::B)),
            0x0D => Some(Op::DEC(IncDecTarget::C)),
            0x15 => Some(Op::DEC(IncDecTarget::D)),
            0x1D => Some(Op::DEC(IncDecTarget::E)),
            0x25 => Some(Op::DEC(IncDecTarget::H)),
            0x2D => Some(Op::DEC(IncDecTarget::L)),
            0x35 => Some(Op::DEC(IncDecTarget::HLIndirect)),
            0x0B => Some(Op::DEC(IncDecTarget::BC)),
            0x1B => Some(Op::DEC(IncDecTarget::DE)),
            0x2B => Some(Op::DEC(IncDecTarget::HL)),
            0x3B => Some(Op::DEC(IncDecTarget::SP)),

            0xC3 => {
                let op = Op::JP(Condition::None, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xC2 => {
                let op = Op::JP(Condition::NZ, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xD2 => {
                let op = Op::JP(Condition::NC, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xCA => {
                let op = Op::JP(Condition::Z, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xDA => {
                let op = Op::JP(Condition::C, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xE9 => {
                let op = Op::JPHLIndirect;
                pc += 1;
                Some(op)
            }

            0x18 => {
                let op = Op::JR(Condition::None, mmu.byte(pc + 1));
                pc += 1;
                Some(op)
            }
            0x20 => {
                let op = Op::JR(Condition::NZ, mmu.byte(pc + 1));
                pc += 1;
                Some(op)
            }
            0x30 => {
                let op = Op::JR(Condition::NC, mmu.byte(pc + 1));
                pc += 1;
                Some(op)
            }
            0x28 => {
                let op = Op::JR(Condition::Z, mmu.byte(pc + 1));
                pc += 1;
                Some(op)
            }
            0x38 => {
                let op = Op::JR(Condition::C, mmu.byte(pc + 1));
                pc += 1;
                Some(op)
            }

            0xCD => {
                let op = Op::CALL(Condition::None, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xC4 => {
                let op = Op::CALL(Condition::NZ, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xD4 => {
                let op = Op::CALL(Condition::NC, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xCC => {
                let op = Op::CALL(Condition::Z, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }
            0xDC => {
                let op = Op::CALL(Condition::C, mmu.word(pc + 1));
                pc += 1;
                Some(op)
            }

            // ADD SP, i8
            0xe8 => Some(Op::ADDSPi8),

            _ => None,
        }?;

        Some((op, pc + 1))
    }

    fn op_from_prefix(mmu: &mut Mmu, pc: u16) -> Option<Op> {
        let byte = mmu.byte(pc);

        match byte {
            0x00 => Some(Op::RLC(PrefixTarget::B)),
            0x01 => Some(Op::RLC(PrefixTarget::C)),
            0x02 => Some(Op::RLC(PrefixTarget::D)),
            0x03 => Some(Op::RLC(PrefixTarget::E)),
            0x04 => Some(Op::RLC(PrefixTarget::H)),
            0x05 => Some(Op::RLC(PrefixTarget::L)),
            0x06 => Some(Op::RLC(PrefixTarget::HLIndirect)),
            0x07 => Some(Op::RLC(PrefixTarget::A)),

            0x08 => Some(Op::RRC(PrefixTarget::B)),
            0x09 => Some(Op::RRC(PrefixTarget::C)),
            0x0A => Some(Op::RRC(PrefixTarget::D)),
            0x0B => Some(Op::RRC(PrefixTarget::E)),
            0x0C => Some(Op::RRC(PrefixTarget::H)),
            0x0D => Some(Op::RRC(PrefixTarget::L)),
            0x0E => Some(Op::RRC(PrefixTarget::HLIndirect)),
            0x0F => Some(Op::RRC(PrefixTarget::A)),

            0x10 => Some(Op::RL(PrefixTarget::B)),
            0x11 => Some(Op::RL(PrefixTarget::C)),
            0x12 => Some(Op::RL(PrefixTarget::D)),
            0x13 => Some(Op::RL(PrefixTarget::E)),
            0x14 => Some(Op::RL(PrefixTarget::H)),
            0x15 => Some(Op::RL(PrefixTarget::L)),
            0x16 => Some(Op::RL(PrefixTarget::HLIndirect)),
            0x17 => Some(Op::RL(PrefixTarget::A)),

            0x18 => Some(Op::RR(PrefixTarget::B)),
            0x19 => Some(Op::RR(PrefixTarget::C)),
            0x1A => Some(Op::RR(PrefixTarget::D)),
            0x1B => Some(Op::RR(PrefixTarget::E)),
            0x1C => Some(Op::RR(PrefixTarget::H)),
            0x1D => Some(Op::RR(PrefixTarget::L)),
            0x1E => Some(Op::RR(PrefixTarget::HLIndirect)),
            0x1F => Some(Op::RR(PrefixTarget::A)),

            0x20 => Some(Op::SLA(PrefixTarget::B)),
            0x21 => Some(Op::SLA(PrefixTarget::C)),
            0x22 => Some(Op::SLA(PrefixTarget::D)),
            0x23 => Some(Op::SLA(PrefixTarget::E)),
            0x24 => Some(Op::SLA(PrefixTarget::H)),
            0x25 => Some(Op::SLA(PrefixTarget::L)),
            0x26 => Some(Op::SLA(PrefixTarget::HLIndirect)),
            0x27 => Some(Op::SLA(PrefixTarget::A)),

            0x28 => Some(Op::SRA(PrefixTarget::B)),
            0x29 => Some(Op::SRA(PrefixTarget::C)),
            0x2A => Some(Op::SRA(PrefixTarget::D)),
            0x2B => Some(Op::SRA(PrefixTarget::E)),
            0x2C => Some(Op::SRA(PrefixTarget::H)),
            0x2D => Some(Op::SRA(PrefixTarget::L)),
            0x2E => Some(Op::SRA(PrefixTarget::HLIndirect)),
            0x2F => Some(Op::SRA(PrefixTarget::A)),

            0x30 => Some(Op::SWAP(PrefixTarget::B)),
            0x31 => Some(Op::SWAP(PrefixTarget::C)),
            0x32 => Some(Op::SWAP(PrefixTarget::D)),
            0x33 => Some(Op::SWAP(PrefixTarget::E)),
            0x34 => Some(Op::SWAP(PrefixTarget::H)),
            0x35 => Some(Op::SWAP(PrefixTarget::L)),
            0x36 => Some(Op::SWAP(PrefixTarget::HLIndirect)),
            0x37 => Some(Op::SWAP(PrefixTarget::A)),

            0x38 => Some(Op::SRL(PrefixTarget::B)),
            0x39 => Some(Op::SRL(PrefixTarget::C)),
            0x3A => Some(Op::SRL(PrefixTarget::D)),
            0x3B => Some(Op::SRL(PrefixTarget::E)),
            0x3C => Some(Op::SRL(PrefixTarget::H)),
            0x3D => Some(Op::SRL(PrefixTarget::L)),
            0x3E => Some(Op::SRL(PrefixTarget::HLIndirect)),
            0x3F => Some(Op::SRL(PrefixTarget::A)),

            0x40 => Some(Op::BIT(PrefixTarget::B, 0)),
            0x41 => Some(Op::BIT(PrefixTarget::C, 0)),
            0x42 => Some(Op::BIT(PrefixTarget::D, 0)),
            0x43 => Some(Op::BIT(PrefixTarget::E, 0)),
            0x44 => Some(Op::BIT(PrefixTarget::H, 0)),
            0x45 => Some(Op::BIT(PrefixTarget::L, 0)),
            0x46 => Some(Op::BIT(PrefixTarget::HLIndirect, 0)),
            0x47 => Some(Op::BIT(PrefixTarget::A, 0)),

            0x48 => Some(Op::BIT(PrefixTarget::B, 1)),
            0x49 => Some(Op::BIT(PrefixTarget::C, 1)),
            0x4A => Some(Op::BIT(PrefixTarget::D, 1)),
            0x4B => Some(Op::BIT(PrefixTarget::E, 1)),
            0x4C => Some(Op::BIT(PrefixTarget::H, 1)),
            0x4D => Some(Op::BIT(PrefixTarget::L, 1)),
            0x4E => Some(Op::BIT(PrefixTarget::HLIndirect, 1)),
            0x4F => Some(Op::BIT(PrefixTarget::A, 1)),

            0x50 => Some(Op::BIT(PrefixTarget::B, 2)),
            0x51 => Some(Op::BIT(PrefixTarget::C, 2)),
            0x52 => Some(Op::BIT(PrefixTarget::D, 2)),
            0x53 => Some(Op::BIT(PrefixTarget::E, 2)),
            0x54 => Some(Op::BIT(PrefixTarget::H, 2)),
            0x55 => Some(Op::BIT(PrefixTarget::L, 2)),
            0x56 => Some(Op::BIT(PrefixTarget::HLIndirect, 2)),
            0x57 => Some(Op::BIT(PrefixTarget::A, 2)),

            0x58 => Some(Op::BIT(PrefixTarget::B, 3)),
            0x59 => Some(Op::BIT(PrefixTarget::C, 3)),
            0x5A => Some(Op::BIT(PrefixTarget::D, 3)),
            0x5B => Some(Op::BIT(PrefixTarget::E, 3)),
            0x5C => Some(Op::BIT(PrefixTarget::H, 3)),
            0x5D => Some(Op::BIT(PrefixTarget::L, 3)),
            0x5E => Some(Op::BIT(PrefixTarget::HLIndirect, 3)),
            0x5F => Some(Op::BIT(PrefixTarget::A, 3)),

            0x60 => Some(Op::BIT(PrefixTarget::B, 4)),
            0x61 => Some(Op::BIT(PrefixTarget::C, 4)),
            0x62 => Some(Op::BIT(PrefixTarget::D, 4)),
            0x63 => Some(Op::BIT(PrefixTarget::E, 4)),
            0x64 => Some(Op::BIT(PrefixTarget::H, 4)),
            0x65 => Some(Op::BIT(PrefixTarget::L, 4)),
            0x66 => Some(Op::BIT(PrefixTarget::HLIndirect, 4)),
            0x67 => Some(Op::BIT(PrefixTarget::A, 4)),

            0x68 => Some(Op::BIT(PrefixTarget::B, 5)),
            0x69 => Some(Op::BIT(PrefixTarget::C, 5)),
            0x6A => Some(Op::BIT(PrefixTarget::D, 5)),
            0x6B => Some(Op::BIT(PrefixTarget::E, 5)),
            0x6C => Some(Op::BIT(PrefixTarget::H, 5)),
            0x6D => Some(Op::BIT(PrefixTarget::L, 5)),
            0x6E => Some(Op::BIT(PrefixTarget::HLIndirect, 5)),
            0x6F => Some(Op::BIT(PrefixTarget::A, 5)),

            0x70 => Some(Op::BIT(PrefixTarget::B, 6)),
            0x71 => Some(Op::BIT(PrefixTarget::C, 6)),
            0x72 => Some(Op::BIT(PrefixTarget::D, 6)),
            0x73 => Some(Op::BIT(PrefixTarget::E, 6)),
            0x74 => Some(Op::BIT(PrefixTarget::H, 6)),
            0x75 => Some(Op::BIT(PrefixTarget::L, 6)),
            0x76 => Some(Op::BIT(PrefixTarget::HLIndirect, 6)),
            0x77 => Some(Op::BIT(PrefixTarget::A, 6)),

            0x78 => Some(Op::BIT(PrefixTarget::B, 7)),
            0x79 => Some(Op::BIT(PrefixTarget::C, 7)),
            0x7A => Some(Op::BIT(PrefixTarget::D, 7)),
            0x7B => Some(Op::BIT(PrefixTarget::E, 7)),
            0x7C => Some(Op::BIT(PrefixTarget::H, 7)),
            0x7D => Some(Op::BIT(PrefixTarget::L, 7)),
            0x7E => Some(Op::BIT(PrefixTarget::HLIndirect, 7)),
            0x7F => Some(Op::BIT(PrefixTarget::A, 7)),

            0xA8 => Some(Op::RES(PrefixTarget::B, 5)),
            0xA9 => Some(Op::RES(PrefixTarget::C, 5)),
            0xAA => Some(Op::RES(PrefixTarget::D, 5)),
            0xAB => Some(Op::RES(PrefixTarget::E, 5)),
            0xAC => Some(Op::RES(PrefixTarget::H, 5)),
            0xAD => Some(Op::RES(PrefixTarget::L, 5)),
            0xAE => Some(Op::RES(PrefixTarget::HLIndirect, 5)),
            0xAF => Some(Op::RES(PrefixTarget::A, 5)),

            0xB0 => Some(Op::RES(PrefixTarget::B, 6)),
            0xB1 => Some(Op::RES(PrefixTarget::C, 6)),
            0xB2 => Some(Op::RES(PrefixTarget::D, 6)),
            0xB3 => Some(Op::RES(PrefixTarget::E, 6)),
            0xB4 => Some(Op::RES(PrefixTarget::H, 6)),
            0xB5 => Some(Op::RES(PrefixTarget::L, 6)),
            0xB6 => Some(Op::RES(PrefixTarget::HLIndirect, 6)),
            0xB7 => Some(Op::RES(PrefixTarget::A, 6)),

            0xB8 => Some(Op::RES(PrefixTarget::B, 7)),
            0xB9 => Some(Op::RES(PrefixTarget::C, 7)),
            0xBA => Some(Op::RES(PrefixTarget::D, 7)),
            0xBB => Some(Op::RES(PrefixTarget::E, 7)),
            0xBC => Some(Op::RES(PrefixTarget::H, 7)),
            0xBD => Some(Op::RES(PrefixTarget::L, 7)),
            0xBE => Some(Op::RES(PrefixTarget::HLIndirect, 7)),
            0xBF => Some(Op::RES(PrefixTarget::A, 7)),

            0xC0 => Some(Op::SET(PrefixTarget::B, 0)),
            0xC1 => Some(Op::SET(PrefixTarget::C, 0)),
            0xC2 => Some(Op::SET(PrefixTarget::D, 0)),
            0xC3 => Some(Op::SET(PrefixTarget::E, 0)),
            0xC4 => Some(Op::SET(PrefixTarget::H, 0)),
            0xC5 => Some(Op::SET(PrefixTarget::L, 0)),
            0xC6 => Some(Op::SET(PrefixTarget::HLIndirect, 0)),
            0xC7 => Some(Op::SET(PrefixTarget::A, 0)),

            0xC8 => Some(Op::SET(PrefixTarget::B, 1)),
            0xC9 => Some(Op::SET(PrefixTarget::C, 1)),
            0xCA => Some(Op::SET(PrefixTarget::D, 1)),
            0xCB => Some(Op::SET(PrefixTarget::E, 1)),
            0xCC => Some(Op::SET(PrefixTarget::H, 1)),
            0xCD => Some(Op::SET(PrefixTarget::L, 1)),
            0xCE => Some(Op::SET(PrefixTarget::HLIndirect, 1)),
            0xCF => Some(Op::SET(PrefixTarget::A, 1)),

            0xD0 => Some(Op::SET(PrefixTarget::B, 2)),
            0xD1 => Some(Op::SET(PrefixTarget::C, 2)),
            0xD2 => Some(Op::SET(PrefixTarget::D, 2)),
            0xD3 => Some(Op::SET(PrefixTarget::E, 2)),
            0xD4 => Some(Op::SET(PrefixTarget::H, 2)),
            0xD5 => Some(Op::SET(PrefixTarget::L, 2)),
            0xD6 => Some(Op::SET(PrefixTarget::HLIndirect, 2)),
            0xD7 => Some(Op::SET(PrefixTarget::A, 2)),

            0xD8 => Some(Op::SET(PrefixTarget::B, 3)),
            0xD9 => Some(Op::SET(PrefixTarget::C, 3)),
            0xDA => Some(Op::SET(PrefixTarget::D, 3)),
            0xDB => Some(Op::SET(PrefixTarget::E, 3)),
            0xDC => Some(Op::SET(PrefixTarget::H, 3)),
            0xDD => Some(Op::SET(PrefixTarget::L, 3)),
            0xDE => Some(Op::SET(PrefixTarget::HLIndirect, 3)),
            0xDF => Some(Op::SET(PrefixTarget::A, 3)),

            0xE0 => Some(Op::SET(PrefixTarget::B, 4)),
            0xE1 => Some(Op::SET(PrefixTarget::C, 4)),
            0xE2 => Some(Op::SET(PrefixTarget::D, 4)),
            0xE3 => Some(Op::SET(PrefixTarget::E, 4)),
            0xE4 => Some(Op::SET(PrefixTarget::H, 4)),
            0xE5 => Some(Op::SET(PrefixTarget::L, 4)),
            0xE6 => Some(Op::SET(PrefixTarget::HLIndirect, 4)),
            0xE7 => Some(Op::SET(PrefixTarget::A, 4)),

            0xE8 => Some(Op::SET(PrefixTarget::B, 5)),
            0xE9 => Some(Op::SET(PrefixTarget::C, 5)),
            0xEA => Some(Op::SET(PrefixTarget::D, 5)),
            0xEB => Some(Op::SET(PrefixTarget::E, 5)),
            0xEC => Some(Op::SET(PrefixTarget::H, 5)),
            0xED => Some(Op::SET(PrefixTarget::L, 5)),
            0xEE => Some(Op::SET(PrefixTarget::HLIndirect, 5)),
            0xEF => Some(Op::SET(PrefixTarget::A, 5)),

            0xF0 => Some(Op::SET(PrefixTarget::B, 6)),
            0xF1 => Some(Op::SET(PrefixTarget::C, 6)),
            0xF2 => Some(Op::SET(PrefixTarget::D, 6)),
            0xF3 => Some(Op::SET(PrefixTarget::E, 6)),
            0xF4 => Some(Op::SET(PrefixTarget::H, 6)),
            0xF5 => Some(Op::SET(PrefixTarget::L, 6)),
            0xF6 => Some(Op::SET(PrefixTarget::HLIndirect, 6)),
            0xF7 => Some(Op::SET(PrefixTarget::A, 6)),

            0xF8 => Some(Op::SET(PrefixTarget::B, 7)),
            0xF9 => Some(Op::SET(PrefixTarget::C, 7)),
            0xFA => Some(Op::SET(PrefixTarget::D, 7)),
            0xFB => Some(Op::SET(PrefixTarget::E, 7)),
            0xFC => Some(Op::SET(PrefixTarget::H, 7)),
            0xFD => Some(Op::SET(PrefixTarget::L, 7)),
            0xFE => Some(Op::SET(PrefixTarget::HLIndirect, 7)),
            0xFF => Some(Op::SET(PrefixTarget::A, 7)),

            _ => None,
        }
    }
}

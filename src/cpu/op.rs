use super::register::FlagsRegister;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget),
    AFromIndirect(Indirect),
    IndirectFromA(Indirect),
    AFromIndirectFF00u8,
    IndirectFF00u8FromA,
    SPFromHL,
    HLFromSPu8,
    IndirectFromSP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Indirect {
    BCIndirect,
    DEIndirect,
    HLDIndirect,
    HLIIndirect,
    WordIndirect,
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
    D8,
    HLIndirect,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadWordTarget {
    BC,
    DE,
    HL,
    SP,
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
    D8, // Direct 8
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
    NZ, // Not Zero
    NC, // No Carry
    Z,  // Zero
    C,  // Carry
}

// TODO: Impl Rust traits From or Into instead?
impl Condition {
    fn is_satisfied(&self, flags: FlagsRegister) -> bool {
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
    RETI,   // Enables interrupts after return operation
    PREFIX, // 0xCB
    PUSH(StackTarget),
    POP(StackTarget),
    CALL(Condition),
    RET(Condition),
    JR(Condition),
    JP(Condition),
    JPIndirect,
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

    // RLC(Destination),
    // RRC(Destination),
    // RL(Destination),
    // RR(Destination),
    // SLA(Destination),
    // SRA(Destination),
    // SWAP(Destination),
    // SRL(Destination),
    // BIT(u8, RegisterType),
    // RES(u8, RegisterType),
    // SET(u8, RegisterType),
}

impl Op {
    pub fn from_byte(byte: u8, prefix: bool) -> Option<Op> {
        if prefix {
            Op::from_byte_prefix(byte)
        } else {
            Op::from_byte_no_prefix(byte)
        }
    }

    fn from_byte_prefix(byte: u8) -> Option<Op> {
        None
    }

    fn from_byte_no_prefix(byte: u8) -> Option<Op> {
        match byte {
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

            // POP
            0xC1 => Some(Op::POP(StackTarget::BC)),
            0xD1 => Some(Op::POP(StackTarget::DE)),
            0xE1 => Some(Op::POP(StackTarget::HL)),
            0xF1 => Some(Op::POP(StackTarget::AF)),

            // PUSH
            0xC5 => Some(Op::PUSH(StackTarget::BC)),
            0xD5 => Some(Op::PUSH(StackTarget::DE)),
            0xE5 => Some(Op::PUSH(StackTarget::HL)),
            0xF5 => Some(Op::PUSH(StackTarget::AF)),

            // RET
            0xC0 => Some(Op::RET(Condition::NZ)),
            0xD0 => Some(Op::RET(Condition::NC)),
            0xC8 => Some(Op::RET(Condition::Z)),
            0xD8 => Some(Op::RET(Condition::C)),
            0xC9 => Some(Op::RET(Condition::None)),
            0xD9 => Some(Op::RETI),

            // RST 00h..30h
            0xC7 => Some(Op::RST(0x00)),
            0xD7 => Some(Op::RST(0x10)),
            0xE7 => Some(Op::RST(0x20)),
            0xF7 => Some(Op::RST(0x30)),

            // RST 08h..38h
            0xCF => Some(Op::RST(0x08)),
            0xDF => Some(Op::RST(0x18)),
            0xEF => Some(Op::RST(0x28)),
            0xFF => Some(Op::RST(0x38)),

            // LD
            0x01 => Some(Op::LD(LoadType::Word(LoadWordTarget::BC))),
            0x11 => Some(Op::LD(LoadType::Word(LoadWordTarget::DE))),
            0x21 => Some(Op::LD(LoadType::Word(LoadWordTarget::HL))),
            0x31 => Some(Op::LD(LoadType::Word(LoadWordTarget::SP))),

            0x40 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::B,
            ))),
            0x41 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::C,
            ))),
            0x42 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::D,
            ))),
            0x43 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::E,
            ))),
            0x44 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::H,
            ))),
            0x45 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::L,
            ))),
            0x46 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::HLIndirect,
            ))),
            0x47 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::A,
            ))),

            0x48 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::B,
            ))),
            0x49 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::C,
            ))),
            0x4A => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::D,
            ))),
            0x4B => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::E,
            ))),
            0x4C => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::H,
            ))),
            0x4D => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::L,
            ))),
            0x4E => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::HLIndirect,
            ))),
            0x4F => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::A,
            ))),

            0x50 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::B,
            ))),
            0x51 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::C,
            ))),
            0x52 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::D,
            ))),
            0x53 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::E,
            ))),
            0x54 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::H,
            ))),
            0x55 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::L,
            ))),
            0x56 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::HLIndirect,
            ))),
            0x57 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::A,
            ))),

            0x58 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::B,
            ))),
            0x59 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::C,
            ))),
            0x5A => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::D,
            ))),
            0x5B => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::E,
            ))),
            0x5C => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::H,
            ))),
            0x5D => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::L,
            ))),
            0x5E => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::HLIndirect,
            ))),
            0x5F => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::A,
            ))),

            0x60 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::B,
            ))),
            0x61 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::C,
            ))),
            0x62 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::D,
            ))),
            0x63 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::E,
            ))),
            0x64 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::H,
            ))),
            0x65 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::L,
            ))),
            0x66 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::HLIndirect,
            ))),
            0x67 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::A,
            ))),

            0x68 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::B,
            ))),
            0x69 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::C,
            ))),
            0x6A => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::D,
            ))),
            0x6B => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::E,
            ))),
            0x6C => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::H,
            ))),
            0x6D => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::L,
            ))),
            0x6E => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::HLIndirect,
            ))),
            0x6F => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::A,
            ))),

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
            0x77 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::HLIndirect,
                        LoadByteSource::A,
            ))),

            0x78 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::B,
            ))),
            0x79 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::C,
            ))),
            0x7A => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::D,
            ))),
            0x7B => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::E,
            ))),
            0x7C => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::H,
            ))),
            0x7D => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::L,
            ))),
            0x7E => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::HLIndirect,
            ))),
            0x7F => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::A,
            ))),

            0x3E => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::A,
                        LoadByteSource::D8,
            ))),
            0x06 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::B,
                        LoadByteSource::D8,
            ))),
            0x0E => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::C,
                        LoadByteSource::D8,
            ))),
            0x16 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::D,
                        LoadByteSource::D8,
            ))),
            0x1E => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::E,
                        LoadByteSource::D8,
            ))),
            0x26 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::H,
                        LoadByteSource::D8,
            ))),
            0x2E => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::L,
                        LoadByteSource::D8,
            ))),
            0x36 => Some(Op::LD(LoadType::Byte(
                        LoadByteTarget::HLIndirect,
                        LoadByteSource::D8,
            ))),

            0xF2 => Some(Op::LD(LoadType::AFromIndirect(
                        Indirect::FF00PlusC,
            ))),
            0x0A => Some(Op::LD(LoadType::AFromIndirect(
                        Indirect::BCIndirect,
            ))),
            0x1A => Some(Op::LD(LoadType::AFromIndirect(
                        Indirect::DEIndirect,
            ))),
            0x2A => Some(Op::LD(LoadType::AFromIndirect(
                        Indirect::HLIIndirect,
            ))),
            0x3A => Some(Op::LD(LoadType::AFromIndirect(
                        Indirect::HLDIndirect,
            ))),
            0xFA => Some(Op::LD(LoadType::AFromIndirect(
                        Indirect::WordIndirect,
            ))),

            0xE2 => Some(Op::LD(LoadType::IndirectFromA(
                        Indirect::FF00PlusC,
            ))),
            0x02 => Some(Op::LD(LoadType::IndirectFromA(
                        Indirect::BCIndirect,
            ))),
            0x12 => Some(Op::LD(LoadType::IndirectFromA(
                        Indirect::DEIndirect,
            ))),
            0x22 => Some(Op::LD(LoadType::IndirectFromA(
                        Indirect::HLIIndirect,
            ))),
            0x32 => Some(Op::LD(LoadType::IndirectFromA(
                        Indirect::HLDIndirect,
            ))),
            0xEA => Some(Op::LD(LoadType::IndirectFromA(
                        Indirect::WordIndirect,
            ))),

            0xE0 => Some(Op::LD(LoadType::IndirectFF00u8FromA)),
            0xF0 => Some(Op::LD(LoadType::AFromIndirectFF00u8)),

            0x08 => Some(Op::LD(LoadType::IndirectFromSP)),
            0xF9 => Some(Op::LD(LoadType::SPFromHL)),
            0xF8 => Some(Op::LD(LoadType::HLFromSPu8)),

            // ADD A
            0x87 => Some(Op::ADD(ArithmeticTarget::A)),
            0x80 => Some(Op::ADD(ArithmeticTarget::B)),
            0x81 => Some(Op::ADD(ArithmeticTarget::C)),
            0x82 => Some(Op::ADD(ArithmeticTarget::D)),
            0x83 => Some(Op::ADD(ArithmeticTarget::E)),
            0x84 => Some(Op::ADD(ArithmeticTarget::H)),
            0x85 => Some(Op::ADD(ArithmeticTarget::L)),
            0x86 => Some(Op::ADD(ArithmeticTarget::HLIndirect)),
            0xC6 => Some(Op::ADD(ArithmeticTarget::D8)),

            // ADC A
            0x8F => Some(Op::ADC(ArithmeticTarget::A)),
            0x88 => Some(Op::ADC(ArithmeticTarget::B)),
            0x89 => Some(Op::ADC(ArithmeticTarget::C)),
            0x8A => Some(Op::ADC(ArithmeticTarget::D)),
            0x8B => Some(Op::ADC(ArithmeticTarget::E)),
            0x8C => Some(Op::ADC(ArithmeticTarget::H)),
            0x8D => Some(Op::ADC(ArithmeticTarget::L)),
            0x8E => Some(Op::ADC(ArithmeticTarget::HLIndirect)),
            0xCE => Some(Op::ADC(ArithmeticTarget::D8)),

            // SUB A
            0x97 => Some(Op::SUB(ArithmeticTarget::A)),
            0x90 => Some(Op::SUB(ArithmeticTarget::B)),
            0x91 => Some(Op::SUB(ArithmeticTarget::C)),
            0x92 => Some(Op::SUB(ArithmeticTarget::D)),
            0x93 => Some(Op::SUB(ArithmeticTarget::E)),
            0x94 => Some(Op::SUB(ArithmeticTarget::H)),
            0x95 => Some(Op::SUB(ArithmeticTarget::L)),
            0x96 => Some(Op::SUB(ArithmeticTarget::HLIndirect)),
            0xD6 => Some(Op::SUB(ArithmeticTarget::D8)),

            // SBC A
            0x9F => Some(Op::SBC(ArithmeticTarget::A)),
            0x98 => Some(Op::SBC(ArithmeticTarget::B)),
            0x99 => Some(Op::SBC(ArithmeticTarget::C)),
            0x9A => Some(Op::SBC(ArithmeticTarget::D)),
            0x9B => Some(Op::SBC(ArithmeticTarget::E)),
            0x9C => Some(Op::SBC(ArithmeticTarget::H)),
            0x9D => Some(Op::SBC(ArithmeticTarget::L)),
            0x9E => Some(Op::SBC(ArithmeticTarget::HLIndirect)),
            0xDE => Some(Op::SBC(ArithmeticTarget::D8)),

            // AND A
            0xA7 => Some(Op::AND(ArithmeticTarget::A)),
            0xA0 => Some(Op::AND(ArithmeticTarget::B)),
            0xA1 => Some(Op::AND(ArithmeticTarget::C)),
            0xA2 => Some(Op::AND(ArithmeticTarget::D)),
            0xA3 => Some(Op::AND(ArithmeticTarget::E)),
            0xA4 => Some(Op::AND(ArithmeticTarget::H)),
            0xA5 => Some(Op::AND(ArithmeticTarget::L)),
            0xA6 => Some(Op::AND(ArithmeticTarget::HLIndirect)),
            0xE6 => Some(Op::AND(ArithmeticTarget::D8)),

            // XOR A
            0xAF => Some(Op::XOR(ArithmeticTarget::A)),
            0xA8 => Some(Op::XOR(ArithmeticTarget::B)),
            0xA9 => Some(Op::XOR(ArithmeticTarget::C)),
            0xAA => Some(Op::XOR(ArithmeticTarget::D)),
            0xAB => Some(Op::XOR(ArithmeticTarget::E)),
            0xAC => Some(Op::XOR(ArithmeticTarget::H)),
            0xAD => Some(Op::XOR(ArithmeticTarget::L)),
            0xAE => Some(Op::XOR(ArithmeticTarget::HLIndirect)),
            0xEE => Some(Op::XOR(ArithmeticTarget::D8)),

            // OR A
            0xB7 => Some(Op::OR(ArithmeticTarget::A)),
            0xB0 => Some(Op::OR(ArithmeticTarget::B)),
            0xB1 => Some(Op::OR(ArithmeticTarget::C)),
            0xB2 => Some(Op::OR(ArithmeticTarget::D)),
            0xB3 => Some(Op::OR(ArithmeticTarget::E)),
            0xB4 => Some(Op::OR(ArithmeticTarget::H)),
            0xB5 => Some(Op::OR(ArithmeticTarget::L)),
            0xB6 => Some(Op::OR(ArithmeticTarget::HLIndirect)),
            0xF6 => Some(Op::OR(ArithmeticTarget::D8)),

            // CP A
            0xBF => Some(Op::CP(ArithmeticTarget::A)),
            0xB8 => Some(Op::CP(ArithmeticTarget::B)),
            0xB9 => Some(Op::CP(ArithmeticTarget::C)),
            0xBA => Some(Op::CP(ArithmeticTarget::D)),
            0xBB => Some(Op::CP(ArithmeticTarget::E)),
            0xBC => Some(Op::CP(ArithmeticTarget::H)),
            0xBD => Some(Op::CP(ArithmeticTarget::L)),
            0xBE => Some(Op::CP(ArithmeticTarget::HLIndirect)),
            0xFE => Some(Op::CP(ArithmeticTarget::D8)),

            // ADD HL
            0x09 => Some(Op::ADDHL(ADDHLTarget::BC)),
            0x19 => Some(Op::ADDHL(ADDHLTarget::DE)),
            0x29 => Some(Op::ADDHL(ADDHLTarget::HL)),
            0x39 => Some(Op::ADDHL(ADDHLTarget::SP)),

            // INC
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

            // DEC
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


            // JP
            0xC3 => Some(Op::JP(Condition::None)),
            0xC2 => Some(Op::JP(Condition::NZ)),
            0xD2 => Some(Op::JP(Condition::NC)),
            0xCA => Some(Op::JP(Condition::Z)),
            0xDA => Some(Op::JP(Condition::C)),
            0xE9 => Some(Op::JPIndirect),

            // JR
            0x18 => Some(Op::JR(Condition::None)),
            0x20 => Some(Op::JR(Condition::NZ)),
            0x30 => Some(Op::JR(Condition::NC)),
            0x28 => Some(Op::JR(Condition::Z)),
            0x38 => Some(Op::JR(Condition::C)),


            // CALL
            0xCD => Some(Op::CALL(Condition::None)),
            0xC4 => Some(Op::CALL(Condition::NZ)),
            0xD4 => Some(Op::CALL(Condition::NC)),
            0xCC => Some(Op::CALL(Condition::Z)),
            0xDC => Some(Op::CALL(Condition::C)),

            // ADD SP, i8
            0xe8 => Some(Op::ADDSPi8),

            _ => None,
        }
    }
}

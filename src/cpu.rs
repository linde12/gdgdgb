use crate::error::GBError;
use crate::mmu::Mmu;
use crate::register::{Register, RegisterType8, RegisterType16, ByteOrWord, Flag, FlagsRegister};

const IO_REGISTER_OFFSET: usize = 0xff00;

// Used to algorithmically parse opcode ranges
static DEFAULT_DST_REGISTER_ORDER: [Destination; 8] = [
    Destination::Direct(Target::Register8(RegisterType8::B)),
    Destination::Direct(Target::Register8(RegisterType8::C)),
    Destination::Direct(Target::Register8(RegisterType8::D)),
    Destination::Direct(Target::Register8(RegisterType8::E)),
    Destination::Direct(Target::Register8(RegisterType8::H)),
    Destination::Direct(Target::Register8(RegisterType8::L)),
    Destination::Indirect(IndirectTarget::Register16(RegisterType16::HL)), // (HL)
    Destination::Direct(Target::Register8(RegisterType8::A)),
];

static DEFAULT_SRC_REGISTER_ORDER: [Source; 8] = [
    Source::Direct(Target::Register8(RegisterType8::B)),
    Source::Direct(Target::Register8(RegisterType8::C)),
    Source::Direct(Target::Register8(RegisterType8::D)),
    Source::Direct(Target::Register8(RegisterType8::E)),
    Source::Direct(Target::Register8(RegisterType8::H)),
    Source::Direct(Target::Register8(RegisterType8::L)),
    Source::Indirect(IndirectTarget::Register16(RegisterType16::HL)), // (HL)
    Source::Direct(Target::Register8(RegisterType8::A)),
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
    Indirect(IndirectTarget),     // A pointer to an address, either from register or an address location
    Indexed(IndexedTarget, usize), // Value of target+offset, where target can be a value in a register or a u16 and offset is a u16
}

// See https://www.cs.helsinki.fi/u/kerola/tito/koksi_doc/memaddr.html
#[derive(Debug, Copy, Clone)]
pub enum Source {
    Immediate8(u8),
    Immediate16(usize),
    Direct(Target),       // Direct value, either a register or u16 address
    Indirect(IndirectTarget),     // A pointer to an address, either from register or an address location
    Indexed(IndexedTarget, usize), // Value of target+offset, where target can be a value in a register or a u16 and offset is a u16
    Offset(RegisterType16, i8) // E.g. 0xF8 => LD HL, SP+i8
}

#[derive(Debug, Copy, Clone)]
pub enum Target {
    Register16(RegisterType16),
    Register8(RegisterType8),
    Address(usize),
}

#[derive(Debug, Copy, Clone)]
pub enum IndirectTarget { // e.g. (HL) or ($ff00)
    Register16(RegisterType16),
    Address(usize),
}

#[derive(Debug, Copy, Clone)]
pub enum IndexedTarget { // e.g. (HL) or ($ff00)
    Register8(RegisterType8),
    Immediate8(usize),
}

#[derive(Debug, Copy, Clone)]
pub enum Conditional {
    NZ, // Not Zero
    NC, // No Carry
    Z,  // Zero
    C,  // Carry
}

#[derive(Debug)]
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
    CALL(Option<Conditional>, usize),
    RET(Option<Conditional>),
    JR(Option<Conditional>, i8),
    INC(Destination),
    DEC(Destination),
    LD(Destination, Source), // Load Operand 2 into Operand 1
    LDi8(Destination, Source),
    ADD(Destination, Source),
    ADDi8(RegisterType16, i8),
    ADC(Destination, Source),
    SUB(Destination, Source),
    SBC(Destination, Source),
    AND(Destination, Source),
    OR(Destination, Source),
    XOR(Destination, Source),
    CP(Destination, Source),
    RST(u8),
    RLC(Destination),
    RRC(Destination),
    RL(Destination),
    RR(Destination),
    SLA(Destination),
    SRA(Destination),
    SWAP(Destination),
    SRL(Destination),
    BIT(u8, Source),
    RES(u8, Source),
    SET(u8, Source),
}
pub struct Cpu {
    mmu: Mmu,
    reg: Register,
    stack: [usize; 16],
    // PREFIX, 0xCB
    cb: bool,
}

impl Cpu {
    pub fn new(mmu: Mmu) -> Cpu {
        Cpu {
            reg: Register::new(),
            mmu,
            stack: [0usize; 16],
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
    pub fn pc(&self) -> usize {
        self.reg.pc
    }

    pub fn byte(&mut self) -> u8 {
        let value = self.mmu.byte(self.reg.pc);
        self.reg.pc += 1;
        value
    }

    pub fn word(&mut self) -> usize {
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
            0xc0 => Ok(Op::RET(Some(Conditional::NZ))),
            0xd0 => Ok(Op::RET(Some(Conditional::NC))),
            0xc8 => Ok(Op::RET(Some(Conditional::Z))),
            0xd8 => Ok(Op::RET(Some(Conditional::C))),
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
                    Destination::Indirect(IndirectTarget::Register16(RegisterType16::HL)),
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
                    DEFAULT_DST_REGISTER_ORDER[dst_index],
                    DEFAULT_SRC_REGISTER_ORDER[src_index],
                ))
            }

            0x02 => Ok(Op::LD(
                Destination::Indirect(IndirectTarget::Register16(RegisterType16::BC)),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),

            0x12 => Ok(Op::LD(
                Destination::Indirect(IndirectTarget::Register16(RegisterType16::DE)),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),

            0xEA => {
                let value = self.word() as usize;
                Ok(Op::LD(
                    Destination::Direct(Target::Address(value)),
                    Source::Direct(Target::Register8(RegisterType8::A)),
                ))
            }

            // LD A (nn)
            0x0a | 0x1a | 0x2a | 0x3a => {
                let register_order: [Source; 4] = [
                    Source::Indirect(IndirectTarget::Register16(RegisterType16::BC)),
                    Source::Indirect(IndirectTarget::Register16(RegisterType16::DE)),
                    Source::Indirect(IndirectTarget::Register16(RegisterType16::HLI)),
                    Source::Indirect(IndirectTarget::Register16(RegisterType16::HLD)),
                ];
                let index = ((op & 0xF0) >> 4) as usize;

                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    register_order[index],
                ))
            }

            // LD A, u8
            0x3e => {
                let value = self.byte();
                Ok(Op::LD(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // LD A, (C)
            // Same as LD A, (0xff00 + C)
            0xF2 => Ok(Op::LD(
                Destination::Direct(Target::Register8(RegisterType8::A)),
                Source::Indexed(IndexedTarget::Register8(RegisterType8::C), IO_REGISTER_OFFSET),
            )),

            // LD (C), A
            // Same as LD (0xff00 + C), A
            0xE2 => Ok(Op::LD(
                Destination::Indexed(IndexedTarget::Register8(RegisterType8::C), IO_REGISTER_OFFSET),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),
            // LD (HL-), A
            0x32 => Ok(Op::LD(
                Destination::Indirect(IndirectTarget::Register16(RegisterType16::HLD)),
                Source::Direct(Target::Register8(RegisterType8::A)),
            )),

            // LD (HLI), A
            0x22 => Ok(Op::LD(
                Destination::Indirect(IndirectTarget::Register16(RegisterType16::HLI)),
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
                Ok(Op::ADD(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // ADC A, $n
            0x88..=0x8F => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::ADC(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // SUB A, $n
            0x90..=0x97 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::SUB(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // SBC A, $n
            0x98..=0x9F => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::SBC(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
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
                Ok(Op::AND(
                    Destination::Direct(Target::Register16(RegisterType16::HL)),
                    register_order[index],
                ))
            }

            // AND A, $n
            0xA0..=0xA7 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::AND(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // XOR A, $n
            0xA8..=0xAF => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::XOR(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // OR A, $n
            0xB0..=0xB7 => {
                let index = (op & 0x0F) as usize; // Low nibble will be our index
                Ok(Op::OR(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
            }

            // CP A, $n
            0xB8..=0xBF => {
                let index = ((op & 0x0F) - 8) as usize; // Low nibble with offset -8 will be our index
                Ok(Op::CP(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    DEFAULT_SRC_REGISTER_ORDER[index],
                ))
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
                    Destination::Indirect(IndirectTarget::Register16(RegisterType16::HL)),
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
                    Destination::Indirect(IndirectTarget::Register16(RegisterType16::HL)),
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
                Ok(Op::JR(Some(Conditional::NZ), value))
            }

            // JR NC, i8
            0x30 => {
                let value = self.byte() as i8;
                Ok(Op::JR(Some(Conditional::NC), value))
            }

            // JR Z, i8
            0x28 => {
                let value = self.byte() as i8;
                Ok(Op::JR(Some(Conditional::Z), value))
            }

            // JR C, i8
            0x38 => {
                let value = self.byte() as i8;
                Ok(Op::JR(Some(Conditional::C), value))
            }

            // CALL $cond, u16
            0xc4 => {
                let value = self.word();
                Ok(Op::CALL(Some(Conditional::NZ), value))
            }

            0xd4 => {
                let value = self.word();
                Ok(Op::CALL(Some(Conditional::NC), value))
            }

            0xcc => {
                let value = self.word();
                Ok(Op::CALL(Some(Conditional::Z), value))
            }

            0xdc => {
                let value = self.word();
                Ok(Op::CALL(Some(Conditional::C), value))
            }

            0xcd => {
                let value = self.word();
                Ok(Op::CALL(None, value))
            }

            // ADC A, u8
            0xce => {
                let value = self.byte();
                Ok(Op::ADC(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // SBC A, u8
            0xde => {
                let value = self.byte();
                Ok(Op::SBC(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // XOR A, u8
            0xee => {
                let value = self.byte();
                Ok(Op::XOR(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // CP A, u8
            0xfe => {
                let value = self.byte();
                Ok(Op::CP(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // ADD A, u8
            0xc6 => {
                let value = self.byte();
                Ok(Op::ADD(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // SUB A, u8
            0xd6 => {
                let value = self.byte();
                Ok(Op::SUB(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // AND A, u8
            0xe6 => {
                let value = self.byte();
                Ok(Op::AND(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
            }

            // OR A, u8
            0xf6 => {
                let value = self.byte();
                Ok(Op::OR(
                    Destination::Direct(Target::Register8(RegisterType8::A)),
                    Source::Immediate8(value),
                ))
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
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::RLC(reg))
            }

            // RRC B->A
            0x08..=0x0F => {
                let low = op & 0x0F;
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::RRC(reg))
            }

            // RL B->A
            0x10..=0x17 => {
                let low = op & 0x0F;
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::RL(reg))
            }

            // RR B->A
            0x18..=0x1F => {
                let low = op & 0x0F;
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::RR(reg))
            }

            // SLA B->A
            0x20..=0x27 => {
                let low = op & 0x0F;
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::SLA(reg))
            }

            // SLR B->A
            0x28..=0x2F => {
                let low = op & 0x0F;
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::SRA(reg))
            }

            // SWAP B->A
            0x30..=0x37 => {
                let low = op & 0x0F;
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::SWAP(reg))
            }

            // SRL B->A
            0x38..=0x3F => {
                let low = op & 0x0F;
                let reg = DEFAULT_DST_REGISTER_ORDER[low as usize];
                Ok(Op::SRL(reg))
            }

            // BIT index, $n
            0x40..=0x7f => {
                // increment index in 8 step intervals, starting from 0
                let dst_index = ((op - 0x40) / 8) as u8;

                // use low nibble as source index
                let src_index = ((op & 0x0F) % 8) as usize;

                Ok(Op::BIT(dst_index, DEFAULT_SRC_REGISTER_ORDER[src_index]))
            }

            // RES index, $n
            0x80..=0xbf => {
                // increment index in 8 step intervals, starting from 0
                let dst_index = ((op - 0x80) / 8) as u8;

                // use low nibble as source index
                let src_index = ((op & 0x0F) % 8) as usize;

                Ok(Op::RES(dst_index, DEFAULT_SRC_REGISTER_ORDER[src_index]))
            }

            // SET index, $n
            0xc0..=0xff => {
                // increment index in 8 step intervals, starting from 0
                let dst_index = ((op - 0xc0) / 8) as u8;

                // use low nibble as source index
                let src_index = ((op & 0x0F) % 8) as usize;

                Ok(Op::SET(dst_index, DEFAULT_SRC_REGISTER_ORDER[src_index]))
            }
        }
    }

    pub fn execute_instruction(&mut self, instruction: Op) {
        match instruction {
            Op::JR(flag, offset) => self.jr(flag, offset),
            Op::BIT(n, src) => self.bit(n, src),
            Op::LD(dst, src) => self.ld(dst, src),
            _ => { 0 }
        };
    }

    fn jr(&mut self, flag: Option<Conditional>, offset: i8) -> u8 {
        let op_pc = self.reg.pc - 1;
        let addr = op_pc.wrapping_add(offset as usize);
        if let Some(flag) = flag {
            let cpu_flags: FlagsRegister = self.reg.f.into();
            match flag {
                Conditional::NZ => { if !cpu_flags.z { self.reg.pc = addr; } }
                Conditional::Z => { if cpu_flags.z { self.reg.pc = addr; } }
                Conditional::NC => { if !cpu_flags.c { self.reg.pc = addr; } }
                Conditional::C => { if cpu_flags.c { self.reg.pc = addr; } }
            };
            12
        } else {
            self.reg.pc = addr;
            8
        }
    }

    fn bit(&mut self, n: u8, source: Source) -> u8 {
        match source {
            Source::Direct(Target::Register8(reg)) => {
                let reg = self.reg.reg8(reg);
                let cpu_flags: FlagsRegister = self.reg.f.into();
                if reg >> n & 0b1 == 0 {
                    self.reg.set_flag(Flag::Zero, false);
                }
                self.reg.set_flag(Flag::Negative, false);
                self.reg.set_flag(Flag::HalfCarry, true);
                8
            }
            _ => panic!("invalid BIT target"),
        }
    }

    fn ld(&mut self, dst: Destination, src: Source) -> u8 {
        let src_value = self.value_from_source(src);
        println!("source value is {:02x}", src_value);

        match dst {
            Destination::Direct(target) => {
                match target {
                    Target::Register8(reg) => self.reg.set_reg8(reg, src_value as u8),
                    Target::Register16(reg) => self.reg.set_reg16(reg, src_value),
                    Target::Address(addr) => self.mmu.write_byte(addr, src_value as u8),
                }
            }
            Destination::Indirect(target) => {
                // TODO: HLD, HLI
                match target {
                    IndirectTarget::Register16(reg) => {
                        let addr = self.reg.reg16(reg);
                        self.mmu.write_word(addr, src_value);
                    }
                    IndirectTarget::Address(addr) => {
                        self.mmu.write_word(addr, src_value);
                    }
                }
            }
            Destination::Indexed(target, addr) => {
                match target {
                    IndexedTarget::Register8(reg) => {
                        let addr_offset = self.reg.reg8(reg);
                        self.mmu.write_byte(addr + addr_offset as usize, src_value as u8);
                    }
                    IndexedTarget::Immediate8(addr_offset) => {
                        self.mmu.write_byte(addr + addr_offset as usize, src_value as u8);
                    }
                }
            }
        }

        8
    }

    fn value_from_source(&self, src: Source) -> usize {
        match src {
            Source::Immediate8(n) => n as usize,
            Source::Immediate16(nn) => nn,
            Source::Direct(target) => self.direct_from_target(target),
            Source::Indirect(target) => self.indirect_from_target(target) as usize,
            Source::Indexed(target, offset) => self.indexed_from_target(target, offset) as usize,
            Source::Offset(reg, offset) => { let v = self.reg.reg16(reg); v.wrapping_add(offset as usize) }
        }
    }

    fn direct_from_target(&self, target: Target) -> usize {
        match target {
            Target::Register8(reg) => self.reg.reg8(reg) as usize,
            Target::Register16(reg) => self.reg.reg16(reg),
            Target::Address(addr) => addr,
        }
    }

    fn indirect_from_target(&self, target: IndirectTarget) -> u8 {
        match target {
            IndirectTarget::Register16(reg) => { let addr = self.reg.reg16(reg); self.mmu.byte(addr) }
            IndirectTarget::Address(addr) => self.mmu.byte(addr),
        }
    }

    fn indexed_from_target(&self, target: IndexedTarget, offset_addr: usize) -> u8 {
        match target {

                // Target::Register8(reg) => ByteOrWord::Byte(self.value_from_reg(reg)),
                // // TODO: Refactor byte/word to always return, not Result
                // Target::Address(addr) => ByteOrWord::Byte(self.mmu.byte(addr + offset)),
            IndexedTarget::Register8(reg) => { let offset = self.reg.reg8(reg); self.mmu.byte(offset as usize + offset_addr) }
            IndexedTarget::Immediate8(n) => self.mmu.byte(n + offset_addr),
        }
    }

    pub fn clear_vram(&mut self) -> ProgramCounter {
        self.mmu.clear_vram();
        ProgramCounter::Next
    }
}

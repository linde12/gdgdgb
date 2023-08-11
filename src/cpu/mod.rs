pub mod op;
pub mod register;
use crate::error::GBError;
use crate::mmu::Mmu;
use op::*;
use register::{Flag, FlagsRegister, Register};
use std::fmt;

/// Performs ALU operation on the passed `ArithmeticTarget`. The second parameter specifies the
/// operation to be performed.
/// # Example:
/// ```rust
/// arithmetic!(target, self.xor);
/// ```
macro_rules! arithmetic {
    ($target:ident, $self:ident.$fn:ident) => {{
        match $target {
            ArithmeticTarget::A => $self.$fn($self.reg.a),
            ArithmeticTarget::B => $self.$fn($self.reg.b),
            ArithmeticTarget::C => $self.$fn($self.reg.c),
            ArithmeticTarget::D => $self.$fn($self.reg.d),
            ArithmeticTarget::E => $self.$fn($self.reg.e),
            ArithmeticTarget::H => $self.$fn($self.reg.h),
            ArithmeticTarget::L => $self.$fn($self.reg.l),
            ArithmeticTarget::D8(byte) => {
                $self.$fn(byte);
            }
            ArithmeticTarget::HLIndirect => {
                let value = $self.mmu.byte($self.reg.hl());
                $self.$fn(value);
            }
        };

        match $target {
            ArithmeticTarget::D8(_) => ($self.pc, 8),
            ArithmeticTarget::HLIndirect => ($self.pc, 8),
            _ => ($self.pc, 4),
        }
    }};
}

/// Performs prefix operations on the passed `PrefixTarget`. The second parameter specifies the
/// operation to be performed and the third specifies the bit position.
/// # Example:
/// ```rust
/// prefix_test!(target, self.bit_test, 0);
/// ```
macro_rules! prefix_test {
    ($target:ident, $self:ident.$fn:ident, $pos:ident) => {{
        match $target {
            PrefixTarget::A => $self.$fn($self.reg.a, $pos),
            PrefixTarget::B => $self.$fn($self.reg.b, $pos),
            PrefixTarget::C => $self.$fn($self.reg.c, $pos),
            PrefixTarget::D => $self.$fn($self.reg.d, $pos),
            PrefixTarget::E => $self.$fn($self.reg.e, $pos),
            PrefixTarget::H => $self.$fn($self.reg.h, $pos),
            PrefixTarget::L => $self.$fn($self.reg.l, $pos),
            PrefixTarget::HLIndirect => {
                let value = $self.mmu.byte($self.reg.hl());
                $self.$fn(value, $pos);
                $self.mmu.write_byte($self.reg.hl(), value);
            }
        };

        let cycles = match $target {
            PrefixTarget::HLIndirect => 16,
            _ => 8,
        };

        ($self.pc, cycles)
    }};
}

/// Performs prefix operations on the passed `PrefixTarget` then updates the PrefixTarget with the
/// new value. The second parameter specifies the operation to be performed. The third and optional
/// parameter is the bit position.
/// # Example:
/// ```rust
/// prefix!(target, self.rot_left_through_carry_zero_flag, 0);
/// ```
macro_rules! prefix {
    ($target:ident, $self:ident.$fn:ident) => {{
        match $target {
            PrefixTarget::A => {
                let v = $self.$fn($self.reg.a);
                $self.reg.a = v;
            }
            PrefixTarget::B => {
                let v = $self.$fn($self.reg.b);
                $self.reg.b = v;
            }
            PrefixTarget::C => {
                let v = $self.$fn($self.reg.c);
                $self.reg.c = v;
            }
            PrefixTarget::D => {
                let v = $self.$fn($self.reg.d);
                $self.reg.d = v;
            }
            PrefixTarget::E => {
                let v = $self.$fn($self.reg.e);
                $self.reg.e = v;
            }
            PrefixTarget::H => {
                let v = $self.$fn($self.reg.h);
                $self.reg.h = v;
            }
            PrefixTarget::L => {
                let v = $self.$fn($self.reg.l);
                $self.reg.l = v;
            }
            PrefixTarget::HLIndirect => {
                let value = $self.mmu.byte($self.reg.hl());
                $self.$fn(value);
                $self.mmu.write_byte($self.reg.hl(), value);
            }
        };

        let cycles = match $target {
            PrefixTarget::HLIndirect => 16,
            _ => 8,
        };
        ($self.pc, cycles)
    }};

    ($target:ident, $self:ident.$fn:ident, $pos:ident) => {{
        match $target {
            PrefixTarget::A => {
                let v = $self.$fn($self.reg.a, $pos);
                $self.reg.a = v;
            }
            PrefixTarget::B => {
                let v = $self.$fn($self.reg.b, $pos);
                $self.reg.b = v;
            }
            PrefixTarget::C => {
                let v = $self.$fn($self.reg.c, $pos);
                $self.reg.c = v;
            }
            PrefixTarget::D => {
                let v = $self.$fn($self.reg.d, $pos);
                $self.reg.d = v;
            }
            PrefixTarget::E => {
                let v = $self.$fn($self.reg.e, $pos);
                $self.reg.e = v;
            }
            PrefixTarget::H => {
                let v = $self.$fn($self.reg.h, $pos);
                $self.reg.h = v;
            }
            PrefixTarget::L => {
                let v = $self.$fn($self.reg.l, $pos);
                $self.reg.l = v;
            }
            PrefixTarget::HLIndirect => {
                let value = $self.mmu.byte($self.reg.hl());
                $self.$fn(value, $pos);
                $self.mmu.write_byte($self.reg.hl(), value);
            }
        };

        let cycles = match $target {
            PrefixTarget::HLIndirect => 16,
            _ => 8,
        };

        ($self.pc, cycles)
    }};
}

pub struct Cpu {
    mmu: Mmu,
    pub reg: Register,
    pub pc: u16,
    pub sp: u16,
}

impl Cpu {
    pub fn new(mmu: Mmu) -> Cpu {
        Cpu {
            reg: Register::new(),
            mmu,
            pc: 0,
            sp: 0,
        }
    }

    pub fn read_instruction(&mut self) -> Result<Op, GBError> {
        // TODO: fix error
        let (op, new_pc) =
            Op::read_op(&mut self.mmu, self.pc).ok_or_else(|| GBError::BadCommand)?;
        self.pc = new_pc;
        Ok(op)
    }

    pub fn execute_instruction(&mut self, instruction: Op) {
        // TODO: use _cycles
        let (next_pc, _cycles) = match instruction {
            Op::JR(cond, rel_addr) => {
                self.jr(cond, rel_addr) // different cycles depending on
                                        // condition
            }
            // Op::BIT(target, pos) => prefix!(target, self.bit_test, pos),
            Op::BIT(target, pos) => prefix_test!(target, self.bit_test, pos),
            Op::LD(load_type) => {
                match load_type {
                    LoadType::Byte(dst, src) => {
                        let src_value = match src {
                            LoadByteSource::A => self.reg.a,
                            LoadByteSource::B => self.reg.b,
                            LoadByteSource::C => self.reg.c,
                            LoadByteSource::D => self.reg.d,
                            LoadByteSource::E => self.reg.e,
                            LoadByteSource::H => self.reg.h,
                            LoadByteSource::L => self.reg.l,
                            LoadByteSource::D8(byte) => byte,
                            LoadByteSource::HLIndirect => self.mmu.byte(self.reg.hl()),
                        };

                        match dst {
                            LoadByteTarget::A => self.reg.a = src_value,
                            LoadByteTarget::B => self.reg.b = src_value,
                            LoadByteTarget::C => self.reg.c = src_value,
                            LoadByteTarget::D => self.reg.d = src_value,
                            LoadByteTarget::E => self.reg.e = src_value,
                            LoadByteTarget::H => self.reg.h = src_value,
                            LoadByteTarget::L => self.reg.l = src_value,
                            LoadByteTarget::HLIndirect => {
                                self.mmu.write_byte(self.reg.hl(), src_value)
                            }
                        };

                        (self.pc, 8)
                    }
                    LoadType::Word(dst, src) => {
                        match (dst, src) {
                            (LoadWordTarget::BC, LoadWordSource::D16(word)) => {
                                self.reg.set_bc(word)
                            }
                            (LoadWordTarget::DE, LoadWordSource::D16(word)) => {
                                self.reg.set_de(word)
                            }
                            (LoadWordTarget::HL, LoadWordSource::D16(word)) => {
                                self.reg.set_hl(word)
                            }
                            (LoadWordTarget::SP, LoadWordSource::D16(word)) => self.sp = word,
                        }

                        (self.pc, 12)
                    }
                    LoadType::AFromIndirect(src) => {
                        self.reg.a = match src {
                            Indirect::BC => self.mmu.byte(self.reg.bc()),
                            Indirect::DE => self.mmu.byte(self.reg.de()),
                            Indirect::HL => self.mmu.byte(self.reg.hl()),
                            Indirect::HLD => {
                                let hl = self.reg.hl();
                                self.reg.set_hl(hl.wrapping_sub(1));
                                self.mmu.byte(hl)
                            }
                            Indirect::HLI => {
                                let hl = self.reg.hl();
                                self.reg.set_hl(hl.wrapping_add(1));
                                self.mmu.byte(hl)
                            }
                            Indirect::Word(word) => self.mmu.byte(word),
                            Indirect::FF00PlusC => self.mmu.byte(0xFF00 + self.reg.c as u16),
                        };

                        match src {
                            Indirect::Word(_) => (self.pc, 16),
                            _ => (self.pc, 8),
                        }
                    }
                    LoadType::IndirectFromA(dst) => {
                        let a = self.reg.a;
                        match dst {
                            Indirect::BC => self.mmu.write_byte(self.reg.bc(), a),
                            Indirect::DE => self.mmu.write_byte(self.reg.de(), a),
                            Indirect::HL => self.mmu.write_byte(self.reg.hl(), a),
                            Indirect::HLD => {
                                let hl = self.reg.hl();
                                self.reg.set_hl(hl.wrapping_sub(1));
                                self.mmu.write_byte(hl, a);
                            }
                            Indirect::HLI => {
                                let hl = self.reg.hl();
                                self.reg.set_hl(hl.wrapping_add(1));
                                self.mmu.write_byte(hl, a);
                            }
                            Indirect::Word(word) => self.mmu.write_byte(word, a),
                            Indirect::FF00PlusC => {
                                self.mmu.write_byte(0xFF00 + self.reg.c as u16, a)
                            }
                        };

                        match dst {
                            Indirect::Word(_) => (self.pc, 16),
                            _ => (self.pc, 8),
                        }
                    }
                    LoadType::AFromIndirectFF00u8(byte) => {
                        self.mmu.write_byte(0xFF00 + byte as u16, self.reg.a);
                        (self.pc, 12)
                    }
                    LoadType::IndirectFF00u8FromA(byte) => {
                        self.reg.a = self.mmu.byte(0xFF00 + byte as u16);
                        (self.pc, 12)
                    }
                    LoadType::SPFromHL => {
                        self.sp = self.reg.hl();
                        (self.pc, 8)
                    }
                    LoadType::HLFromSPu8(byte) => {
                        let byte = byte as u16;
                        self.reg.set_flag(Flag::Zero, false);
                        self.reg.set_flag(Flag::Negative, false);
                        // carry from bit 3 to bit 4?
                        self.reg
                            .set_flag(Flag::HalfCarry, (self.sp & 0x0F) + (byte & 0x0F) > 0xF0);
                        // carry from bit 7 to bit 8?
                        self.reg
                            .set_flag(Flag::Carry, (self.sp & 0xFF) + (byte & 0xFF) > 0xFF);
                        (self.pc, 12)
                    }
                    LoadType::IndirectFromSP(word) => {
                        self.mmu.write_word(word, self.sp);
                        (self.pc, 20)
                    }
                }
            }
            // Op::LD(dst, src) => self.ld(dst, src),
            // Op::LDD(dst, src) => self.ldd(dst, src),
            // Op::LDI(dst, src) => self.ldi(dst, src),
            // // Op::NOP => {}
            // // Op::STOP => {}
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
            // Op::PREFIX => 0,
            Op::PUSH(target) => {
                let value = match target {
                    StackTarget::AF => self.reg.af(),
                    StackTarget::BC => self.reg.bc(),
                    StackTarget::DE => self.reg.de(),
                    StackTarget::HL => self.reg.hl(),
                };
                self.push(value);

                (self.pc, 16)
            }
            Op::POP(target) => {
                let value = self.pop();
                match target {
                    StackTarget::AF => self.reg.set_af(value),
                    StackTarget::BC => self.reg.set_bc(value),
                    StackTarget::DE => self.reg.set_de(value),
                    StackTarget::HL => self.reg.set_hl(value),
                };

                (self.pc, 12)
            }
            Op::CALL(condition, addr) => self.call(condition, addr),
            Op::RET(cond) => self.ret(cond),
            Op::INC(target) => {
                match target {
                    IncDecTarget::A => self.reg.a = self.inc_8bit(self.reg.a),
                    IncDecTarget::B => self.reg.b = self.inc_8bit(self.reg.b),
                    IncDecTarget::C => self.reg.c = self.inc_8bit(self.reg.c),
                    IncDecTarget::D => self.reg.d = self.inc_8bit(self.reg.d),
                    IncDecTarget::E => self.reg.e = self.inc_8bit(self.reg.e),
                    IncDecTarget::H => self.reg.h = self.inc_8bit(self.reg.h),
                    IncDecTarget::L => self.reg.l = self.inc_8bit(self.reg.l),
                    IncDecTarget::HL => {
                        let hl = self.reg.hl();
                        let new_value = self.inc_16bit(hl);
                        self.reg.set_hl(new_value);
                    }
                    IncDecTarget::BC => {
                        let bc = self.reg.bc();
                        let new_value = self.inc_16bit(bc);
                        self.reg.set_bc(new_value);
                    }
                    IncDecTarget::DE => {
                        let de = self.reg.de();
                        let new_value = self.inc_16bit(de);
                        self.reg.set_de(new_value);
                    }
                    IncDecTarget::HLIndirect => {
                        let addr = self.reg.hl();
                        let value = self.mmu.byte(addr);
                        let new_value = self.inc_8bit(value);
                        self.mmu.write_byte(addr, new_value);
                    }
                    IncDecTarget::SP => self.sp = self.inc_16bit(self.sp),
                }

                let cycles = match target {
                    IncDecTarget::HLIndirect => 12,
                    IncDecTarget::HL | IncDecTarget::BC | IncDecTarget::DE | IncDecTarget::SP => 8,
                    _ => 4,
                };

                (self.pc, cycles)
            }
            Op::DEC(target) => {
                match target {
                    IncDecTarget::A => self.reg.a = self.dec_8bit(self.reg.a),
                    IncDecTarget::B => self.reg.b = self.dec_8bit(self.reg.b),
                    IncDecTarget::C => self.reg.c = self.dec_8bit(self.reg.c),
                    IncDecTarget::D => self.reg.d = self.dec_8bit(self.reg.d),
                    IncDecTarget::E => self.reg.e = self.dec_8bit(self.reg.e),
                    IncDecTarget::H => self.reg.h = self.dec_8bit(self.reg.h),
                    IncDecTarget::L => self.reg.l = self.dec_8bit(self.reg.l),
                    IncDecTarget::HL => {
                        let hl = self.reg.hl();
                        let new_value = self.dec_16bit(hl);
                        self.reg.set_hl(new_value);
                    }
                    IncDecTarget::BC => {
                        let bc = self.reg.bc();
                        let new_value = self.dec_16bit(bc);
                        self.reg.set_bc(new_value);
                    }
                    IncDecTarget::DE => {
                        let de = self.reg.de();
                        let new_value = self.dec_16bit(de);
                        self.reg.set_de(new_value);
                    }
                    IncDecTarget::HLIndirect => {
                        let addr = self.reg.hl();
                        let value = self.mmu.byte(addr);
                        let new_value = self.dec_8bit(value);
                        self.mmu.write_byte(addr, new_value);
                    }
                    IncDecTarget::SP => self.sp = self.dec_16bit(self.sp),
                }

                let cycles = match target {
                    IncDecTarget::HLIndirect => 12,
                    IncDecTarget::HL | IncDecTarget::BC | IncDecTarget::DE | IncDecTarget::SP => 8,
                    _ => 4,
                };

                (self.pc, cycles)
            }
            // Op::DEC(dst) => self.dec(dst),
            // Op::LDi8(_, _) => {}
            // Op::ADD(_, _) => {}
            // Op::ADDi8(_, _) => {}
            // Op::ADC(_, _) => {}
            // Op::SUB(_, _) => {}
            // Op::SBC(src) => self.sbc(src, true),
            Op::AND(target) => arithmetic!(target, self.and),
            Op::OR(target) => arithmetic!(target, self.or),
            Op::XOR(target) => arithmetic!(target, self.xor),
            Op::CP(target) => arithmetic!(target, self.cmp),
            // Op::RST(_) => {}
            // Op::RLC(_) => {}
            // Op::RRC(_) => {}
            Op::RL(target) => prefix!(target, self.rot_left_through_carry_zero_flag),
            Op::RR(target) => prefix!(target, self.rot_right_through_carry_zero_flag),
            Op::RLA => {
                self.reg.a = self.rot_left_through_carry_no_zero_flag(self.reg.a);
                (self.pc, 4)
            }
            Op::RRA => {
                self.reg.a = self.rot_right_through_carry_no_zero_flag(self.reg.a);
                (self.pc, 4)
            }
            // Op::SLA(_) => {}
            // Op::SRA(_) => {}
            // Op::SWAP(_) => {}
            // Op::SRL(_) => {}
            // Op::RES(_, _) => {}
            // Op::SET(_, _) => {}
            instr => unimplemented!("instruction {:?} not implemented", instr),
        };

        self.pc = next_pc;
    }

    // TODO: test
    fn cmp(&mut self, value: u8) {
        self.reg.set_flag(Flag::Zero, self.reg.a == value);
        self.reg.set_flag(Flag::Negative, true);
        // cmp is done by subtracting value from A and checking if it equals zero
        // so if value is greater than A we will end up with something less than zero, thus
        // carrying
        self.reg
            .set_flag(Flag::HalfCarry, (self.reg.a & 0x0F) < (value & 0x0F));
        self.reg.set_flag(Flag::Carry, self.reg.a < value);
    }

    // TODO: test
    fn xor(&mut self, value: u8) {
        self.reg.a = self.reg.a ^ value;
        self.reg.set_flag(Flag::Zero, self.reg.a == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
        self.reg.set_flag(Flag::Carry, false);
    }

    // TODO: test
    fn or(&mut self, value: u8) {
        self.reg.a = self.reg.a | value;
        self.reg.set_flag(Flag::Zero, self.reg.a == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
        self.reg.set_flag(Flag::Carry, false);
    }

    // TODO: test
    fn and(&mut self, value: u8) {
        self.reg.a = self.reg.a & value;
        self.reg.set_flag(Flag::Zero, self.reg.a == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, true);
        self.reg.set_flag(Flag::Carry, false);
    }

    // TODO: test
    fn bit_test(&mut self, value: u8, pos: u8) {
        let result = (value >> pos) & 0b1;
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, true);
    }

    // TODO: test
    fn jr(&mut self, cond: Condition, rel_addr: u8) -> (u16, u8) {
        let flags: FlagsRegister = self.reg.f.into();
        if cond.is_satisfied(flags) {
            // let relative_offset = self.peek_byte() as i8;
            // relative offset will be cast to u16, so e.g. -5 will become a large number and
            // wrapping_add will make sure that pc wraps to zero and thus negative relative offsets will work
            let next_pc = self.pc.wrapping_add(rel_addr as i8 as u16);
            println!("next pc will be 0x{:04X}", next_pc);
            (next_pc, 12)
        } else {
            (self.pc, 8)
        }
    }

    // TODO: test
    fn inc_8bit(&mut self, value: u8) -> u8 {
        let result = value.wrapping_add(1);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg
            .set_flag(Flag::HalfCarry, (value & 0x0F) + 1 > 0x0F);
        result
    }

    // TODO: test
    fn inc_16bit(&mut self, value: u16) -> u16 {
        value.wrapping_add(1)
    }

    // TODO: test
    fn dec_8bit(&mut self, value: u8) -> u8 {
        let result = value.wrapping_sub(1);
        self.reg.set_flag(Flag::Zero, result == 0);
        self.reg.set_flag(Flag::Negative, true);
        self.reg.set_flag(Flag::HalfCarry, (value & 0x0F) == 0x0);
        result
    }

    // TODO: test
    fn dec_16bit(&mut self, value: u16) -> u16 {
        value.wrapping_sub(1)
    }

    // TODO: test
    fn call(&mut self, cond: Condition, addr: u16) -> (u16, u8) {
        let flags: FlagsRegister = self.reg.f.into();
        if cond.is_satisfied(flags) {
            self.push(self.pc);
            (addr, 24)
        } else {
            (self.pc, 12)
        }
    }

    // TODO: test
    fn push(&mut self, value: u16) {
        self.sp = self.sp.wrapping_sub(2);
        self.mmu.write_word(self.sp, value);
    }

    // TODO: test
    fn pop(&mut self) -> u16 {
        let value = self.mmu.word(self.sp);
        self.sp += 2;
        value
    }

    fn rot_left_through_carry_zero_flag(&mut self, value: u8) -> u8 {
        self.rot_left_through_carry(value, true)
    }

    fn rot_left_through_carry_no_zero_flag(&mut self, value: u8) -> u8 {
        self.rot_left_through_carry(value, true)
    }

    // TODO: test
    fn rot_left_through_carry(&mut self, value: u8, set_zero: bool) -> u8 {
        let flags: FlagsRegister = self.reg.f.into();
        let carry_bit = if flags.c { 1 } else { 0 };
        // shift everything one step to the left, and set whatever carry was set to to the
        // least significant bit
        let next_value = (value << 1) | carry_bit;
        self.reg.set_flag(Flag::Zero, set_zero && next_value == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
        // shift MSB into carry flag
        self.reg.set_flag(Flag::Carry, value & 0x80 == 0x80);
        next_value
    }

    fn rot_right_through_carry_zero_flag(&mut self, value: u8) -> u8 {
        self.rot_right_through_carry(value, true)
    }

    fn rot_right_through_carry_no_zero_flag(&mut self, value: u8) -> u8 {
        self.rot_right_through_carry(value, false)
    }

    // TODO: test
    fn rot_right_through_carry(&mut self, value: u8, set_zero: bool) -> u8 {
        let flags: FlagsRegister = self.reg.f.into();
        let carry_bit = if flags.c { 1 } else { 0 } << 7;
        // shift everything one step to the right, and set whatever carry was set to to the
        // most significant bit
        let next_value = carry_bit | (value >> 1);
        self.reg.set_flag(Flag::Zero, set_zero && next_value == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
        // shift LSB into carry flag
        self.reg.set_flag(Flag::Carry, value & 0b1 == 0b1);
        next_value
    }

    // fn sbc(&mut self, src: Source, use_carry: bool) -> u8 {
    //     let src_value = self.value_from_source(src) as u8;
    //     let carry = if use_carry && self.reg.flag(Flag::Carry) {
    //         1
    //     } else {
    //         0
    //     };

    //     let a = self.reg.a;
    //     let result = a.wrapping_sub(src_value).wrapping_sub(carry);

    //     self.reg.set_flag(Flag::Zero, result == 0);
    //     self.reg.set_flag(Flag::Negative, true);
    //     self.reg
    //         .set_flag(Flag::HalfCarry, (a & 0x0F) + (src_value & 0x0F) > 0x0F);
    //     self.reg
    //         .set_flag(Flag::Carry, (a as u16) < (src_value as u16) + carry as u16);
    //     self.reg.a = result;

    //     8
    // }

    fn ret(&mut self, cond: Condition) -> (u16, u8) {
        let flags: FlagsRegister = self.reg.f.into();
        if cond.is_satisfied(flags) {
            let cycles = match cond {
                Condition::None => 16,
                _ => 20,
            };

            (self.pop(), cycles) // pop return address
        } else {
            (self.pc, 8)
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
            self.pc,
            self.sp,
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

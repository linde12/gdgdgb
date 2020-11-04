pub mod op;
pub mod register;
use crate::error::GBError;
use crate::mmu::Mmu;
use register::{Flag, Register};
use op::*;
use std::fmt;

macro_rules! arithmetic {
    ($target:ident, $self:ident.$fn:ident) => {
        {
            match $target {
                ArithmeticTarget::A => $self.$fn($self.reg.a),
                ArithmeticTarget::B => $self.$fn($self.reg.b),
                ArithmeticTarget::C => $self.$fn($self.reg.c),
                ArithmeticTarget::D => $self.$fn($self.reg.d),
                ArithmeticTarget::E => $self.$fn($self.reg.e),
                ArithmeticTarget::H => $self.$fn($self.reg.h),
                ArithmeticTarget::L => $self.$fn($self.reg.l),
                ArithmeticTarget::D8 => {
                    let byte = $self.peek_byte();
                    $self.$fn(byte);
                },
                ArithmeticTarget::HLIndirect => {
                    let value = $self.mmu.byte($self.reg.hl());
                    $self.$fn(value);
                }
            };

            match $target {
                ArithmeticTarget::D8  => ($self.pc.wrapping_add(2), 8),
                ArithmeticTarget::HLIndirect => ($self.pc.wrapping_add(1), 8),
                _ => ($self.pc.wrapping_add(1), 4)
            }
        }
    };
}

pub struct Cpu {
    mmu: Mmu,
    pub reg: Register,
    // PREFIX, 0xCB
    cb: bool,
    pub pc: u16,
    pub sp: u16,
}

impl Cpu {
    pub fn new(mmu: Mmu) -> Cpu {
        Cpu {
            reg: Register::new(),
            mmu,
            cb: false,
            pc: 0,
            sp: 0,
        }
    }

    pub fn peek_byte(&mut self) -> u8 {
        self.mmu.byte(self.pc + 1)
    }

    pub fn peek_word(&mut self) -> u16 {
        self.mmu.word(self.pc + 1)
    }

    pub fn read_instruction(&mut self) -> Option<Result<Op, GBError>> {
        let byte = self.mmu.byte(self.pc);
        let is_prefix = byte == 0xCB;
        if is_prefix {
            self.cb = true;
            None
        } else {
            self.cb = false;
            Some(Op::from_byte(byte, self.cb).ok_or_else(|| GBError::UnknownOperation(byte)))
        }
    }

    // fn match_cb(&mut self, op: u8) -> Result<Op, GBError> {
    //     match op {
    //         // RLC B->A
    //         0x00..=0x07 => {
    //             let low = op & 0x0F;
    //             let reg = DESTINATION_REGISTERS[low as usize];
    //             Ok(Op::RLC(reg))
    //         }

    //         // RRC B->A
    //         0x08..=0x0F => {
    //             let low = op & 0x0F;
    //             let reg = DESTINATION_REGISTERS[low as usize];
    //             Ok(Op::RRC(reg))
    //         }

    //         // RL B->A
    //         0x10..=0x17 => {
    //             let low = op & 0x0F;
    //             let reg = DESTINATION_REGISTERS[low as usize];
    //             Ok(Op::RL(reg))
    //         }

    //         // RR B->A
    //         0x18..=0x1F => {
    //             let low = op & 0x0F;
    //             let reg = DESTINATION_REGISTERS[low as usize];
    //             Ok(Op::RR(reg))
    //         }

    //         // SLA B->A
    //         0x20..=0x27 => {
    //             let low = op & 0x0F;
    //             let reg = DESTINATION_REGISTERS[low as usize];
    //             Ok(Op::SLA(reg))
    //         }

    //         // SLR B->A
    //         0x28..=0x2F => {
    //             let low = op & 0x0F;
    //             let reg = DESTINATION_REGISTERS[low as usize];
    //             Ok(Op::SRA(reg))
    //         }

    //         // SWAP B->A
    //         0x30..=0x37 => {
    //             let low = op & 0x0F;
    //             let reg = DESTINATION_REGISTERS[low as usize];
    //             Ok(Op::SWAP(reg))
    //         }

    //         // SRL B->A
    //         0x38..=0x3F => {
    //             let low = op & 0x0F;
    //             let reg = DESTINATION_REGISTERS[low as usize];
    //             Ok(Op::SRL(reg))
    //         }

    //         // BIT index, $n
    //         0x40..=0x7f => {
    //             // increment index in 8 step intervals, starting from 0
    //             let dst_index = ((op - 0x40) / 8) as u8;

    //             // use low nibble as source index
    //             let src_index = ((op & 0x0F) % 8) as usize;

    //             Ok(Op::BIT(dst_index, REGISTERS[src_index]))
    //         }

    //         // RES index, $n
    //         0x80..=0xbf => {
    //             // increment index in 8 step intervals, starting from 0
    //             let dst_index = ((op - 0x80) / 8) as u8;

    //             // use low nibble as source index
    //             let src_index = ((op & 0x0F) % 8) as usize;

    //             Ok(Op::RES(dst_index, REGISTERS[src_index]))
    //         }

    //         // SET index, $n
    //         0xc0..=0xff => {
    //             // increment index in 8 step intervals, starting from 0
    //             let dst_index = ((op - 0xc0) / 8) as u8;

    //             // use low nibble as source index
    //             let src_index = ((op & 0x0F) % 8) as usize;

    //             Ok(Op::SET(dst_index, REGISTERS[src_index]))
    //         }
    //     }
    // }

    pub fn execute_instruction(&mut self, instruction: Op) {
        // TODO: use _cycles
        let (next_pc, _cycles) = match instruction {
            // Op::JR(flag, offset) => self.jr(flag, offset),
            // Op::BIT(n, src) => self.bit(n, src),
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
                            LoadByteSource::D8 => self.peek_byte(),
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
                            LoadByteTarget::HLIndirect => self.mmu.write_byte(self.reg.hl(), src_value),
                        };

                        match src {
                            LoadByteSource::D8 => (self.pc.wrapping_add(2), 8),
                            LoadByteSource::HLIndirect => (self.pc.wrapping_add(1), 8),
                            _ => (self.pc.wrapping_add(1), 4),
                        }
                    }
                    LoadType::Word(dst) => {
                        let word = self.peek_word();
                        match dst {
                            LoadWordTarget::BC => self.reg.set_bc(word),
                            LoadWordTarget::DE => self.reg.set_de(word),
                            LoadWordTarget::HL => self.reg.set_hl(word),
                            LoadWordTarget::SP => self.sp = word,
                        }

                        (self.pc.wrapping_add(3), 12)
                    }
                    LoadType::AFromIndirect(src) => {
                        self.reg.a = match src {
                            Indirect::BCIndirect => self.mmu.byte(self.reg.bc()),
                            Indirect::DEIndirect => self.mmu.byte(self.reg.de()),
                            Indirect::HLDIndirect => {
                                let hl = self.reg.hl();
                                self.reg.set_hl(hl.wrapping_sub(1));
                                self.mmu.byte(hl)
                            }
                            Indirect::HLIIndirect => {
                                let hl = self.reg.hl();
                                self.reg.set_hl(hl.wrapping_add(1));
                                self.mmu.byte(hl)
                            }
                            Indirect::WordIndirect => { let word = self.peek_word(); self.mmu.byte(word) },
                            Indirect::FF00PlusC => self.mmu.byte(0xFF00 + self.reg.c as u16),
                        };

                        match src {
                            Indirect::WordIndirect => (self.pc.wrapping_add(3), 16),
                            _ => (self.pc.wrapping_add(1), 8),
                        }
                    }
                    LoadType::IndirectFromA(dst) => {
                        let a = self.reg.a;
                        match dst {
                            Indirect::BCIndirect => self.mmu.write_byte(self.reg.bc(), a),
                            Indirect::DEIndirect => self.mmu.write_byte(self.reg.de(), a),
                            Indirect::HLDIndirect => {
                                let hl = self.reg.hl();
                                self.reg.set_hl(hl.wrapping_sub(1));
                                self.mmu.write_byte(hl, a);
                            }
                            Indirect::HLIIndirect => {
                                let hl = self.reg.hl();
                                self.reg.set_hl(hl.wrapping_add(1));
                                self.mmu.write_byte(hl, a);
                            }
                            Indirect::WordIndirect => {
                                let word = self.peek_word();
                                self.mmu.write_byte(word, a)
                            }
                            Indirect::FF00PlusC => self.mmu.write_byte(0xFF00 + self.reg.c as u16, a),
                        };

                        match dst {
                            Indirect::WordIndirect => (self.pc.wrapping_add(3), 16),
                            _ => (self.pc.wrapping_add(1), 8),
                        }
                    }
                    LoadType::AFromIndirectFF00u8 => {
                        let byte = self.peek_byte();
                        self.mmu.write_byte(0xFF00 + byte as u16, self.reg.a);
                        (self.pc.wrapping_add(2), 12)
                    }
                    LoadType::IndirectFF00u8FromA => {
                        let byte = self.peek_byte();
                        self.reg.a = self.mmu.byte(0xFF00 + byte as u16);
                        (self.pc.wrapping_add(2), 12)
                    }
                    LoadType::SPFromHL => {
                        let word = self.peek_word();
                        self.mmu.write_word(word, self.sp);
                        (self.pc.wrapping_add(3), 20)
                    }
                    LoadType::HLFromSPu8 => {
                        let byte = self.peek_byte() as u16;
                        self.reg.set_flag(Flag::Zero, false);
                        self.reg.set_flag(Flag::Negative, false);
                        // carry from bit 3 to bit 4?
                        self.reg.set_flag(Flag::HalfCarry, (self.sp & 0x0F) + (byte & 0x0F) > 0xF0);
                        // carry from bit 7 to bit 8?
                        self.reg.set_flag(Flag::Carry, (self.sp & 0xFF) + (byte & 0xFF) > 0xFF);
                        (self.pc.wrapping_add(2), 12)
                    }
                    LoadType::IndirectFromSP => {
                        let word = self.peek_word();
                        self.mmu.write_word(word, self.sp);
                        (self.pc.wrapping_add(3), 20)
                    }
                }
            }
            // Op::LD(dst, src) => self.ld(dst, src),
            // Op::LDD(dst, src) => self.ldd(dst, src),
            // Op::LDI(dst, src) => self.ldi(dst, src),
            // // Op::NOP => {}
            // // Op::STOP => {}
            // Op::RLA => {
            //     let ticks = self.rl(Destination::Direct(Target::Register8(RegisterType8::A)));
            //     self.reg.set_flag(Flag::Zero, false);
            //     ticks
            // }
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
            // Op::PUSH(reg) => self.push(reg),
            // Op::POP(reg) => self.pop(reg),
            // Op::CALL(condition, addr) => self.call(condition, addr),
            // Op::RET(cond) => self.ret(cond),
            // Op::INC(dst) => self.inc(dst),
            // Op::DEC(dst) => self.dec(dst),
            // Op::LDi8(_, _) => {}
            // Op::ADD(_, _) => {}
            // Op::ADDi8(_, _) => {}
            // Op::ADC(_, _) => {}
            // Op::SUB(_, _) => {}
            // Op::SBC(src) => self.sbc(src, true),
            // Op::AND(_, _) => {}
            // Op::OR(_, _) => {}
            Op::XOR(target) => arithmetic!(target, self.xor),
            Op::CP(target) => arithmetic!(target, self.cmp),
            // Op::RST(_) => {}
            // Op::RLC(_) => {}
            // Op::RRC(_) => {}
            // Op::RL(dst) => self.rl(dst),
            // Op::RR(_) => {}
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

    fn cmp(&mut self, value: u8) {
        self.reg.set_flag(Flag::Zero, self.reg.a == value);
        self.reg.set_flag(Flag::Negative, true);
        // cmp is done by subtracting value from A and checking if it equals zero
        // so if value is greater than A we will end up with something less than zero, thus
        // carrying
        self.reg.set_flag(Flag::HalfCarry, (self.reg.a & 0x0F) < (value & 0x0F));
        self.reg.set_flag(Flag::Carry, self.reg.a < value);
    }

    fn xor(&mut self, value: u8) {
        self.reg.a = self.reg.a ^ value;
        self.reg.set_flag(Flag::Zero, self.reg.a == 0);
        self.reg.set_flag(Flag::Negative, false);
        self.reg.set_flag(Flag::HalfCarry, false);
        self.reg.set_flag(Flag::Carry, false);
    }

    // fn jr(&mut self, flag: Option<Condition>, offset: i8) -> u8 {
    //     let op_pc = self.reg.pc;
    //     let addr = op_pc.wrapping_add(offset as usize);
    //     if let Some(flag) = flag {
    //         let cpu_flags: FlagsRegister = self.reg.f.into();
    //         match flag {
    //             Condition::NZ => {
    //                 if !cpu_flags.z {
    //                     self.reg.pc = addr;
    //                 }
    //             }
    //             Condition::Z => {
    //                 if cpu_flags.z {
    //                     self.reg.pc = addr;
    //                 }
    //             }
    //             Condition::NC => {
    //                 if !cpu_flags.c {
    //                     self.reg.pc = addr;
    //                 }
    //             }
    //             Condition::C => {
    //                 if cpu_flags.c {
    //                     self.reg.pc = addr;
    //                 }
    //             }
    //         };
    //         12
    //     } else {
    //         self.reg.pc = addr;
    //         8
    //     }
    // }

    // fn bit(&mut self, n: u8, source: RegisterType) -> u8 {
    //     let value = match source {
    //         RegisterType::Register8(reg) => self.reg.reg8(reg),
    //         // BIT n, (HL)
    //         RegisterType::Register16(RegisterType16::HL) => {
    //             let addr = self.reg.hl();
    //             self.mmu.byte(addr as usize)
    //         }
    //         _ => panic!("invalid BIT target"),
    //     };

    //     let is_bit_zero = value & (1 << n) == 0;
    //     if is_bit_zero {
    //         self.reg.set_flag(Flag::Zero, true);
    //     }
    //     self.reg.set_flag(Flag::Negative, false);
    //     self.reg.set_flag(Flag::HalfCarry, true);

    //     8
    // }

    // fn ld(&mut self, dst: Destination, src: Source) -> u8 {
    //     let src_value = self.value_from_source(src);
    //     self.write_into(dst, src_value);
    //     8
    // }

    // fn ldd(&mut self, dst: Destination, src: Source) -> u8 {
    //     let src_value = self.value_from_source(src);
    //     self.write_into(dst, src_value);
    //     self.reg.dec_hl();
    //     8
    // }

    // fn ldi(&mut self, dst: Destination, src: Source) -> u8 {
    //     let src_value = self.value_from_source(src);
    //     self.write_into(dst, src_value);
    //     self.reg.inc_hl();
    //     8
    // }

    // fn call(&mut self, cond: Option<Condition>, addr: u16) -> u8 {
    //     if let Some(condition) = cond {
    //         let cpu_flags: FlagsRegister = self.reg.f.into();
    //         if condition.is_satisfied(cpu_flags) {
    //             self.push_stack(self.reg.pc as u16);
    //             self.reg.pc = addr as usize;
    //             return 24;
    //         }
    //         12
    //     } else {
    //         self.push_stack(self.reg.pc as u16);
    //         self.reg.pc = addr as usize;
    //         24
    //     }
    // }

    // fn xor(&mut self, src: Source) -> u8 {
    //     let src_value = self.value_from_source(src) as u8;

    //     self.reg.a = src_value ^ self.reg.a;
    //     if self.reg.a == 0 {
    //         self.reg.set_flag(Flag::Zero, false);
    //     }

    //     8
    // }

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

    // fn inc(&mut self, dst: Destination) -> u8 {
    //     let value = self.value_from_destination(dst) as u8;
    //     let result = value.wrapping_add(1);
    //     self.reg.set_flag(Flag::Zero, result == 0);
    //     // if lower nibble (e.g. 1111) + 1 is greater than 0x0F(1111), the upper nibble will be
    //     // affected (e.g. 0001 0000) and we set half carry
    //     self.reg
    //         .set_flag(Flag::HalfCarry, (value & 0x0F) + 1 > 0x0F);
    //     self.reg.set_flag(Flag::Negative, false);
    //     self.write_into(dst, result as u16);

    //     8
    // }

    // fn dec(&mut self, dst: Destination) -> u8 {
    //     let value = self.value_from_destination(dst) as u8;
    //     let result = value.wrapping_sub(1);
    //     self.reg.set_flag(Flag::Zero, result == 0);
    //     // if lower nibble is 0000 and we are trying to subtract one, then we have to borrow from
    //     // upper nibble. So we set HalfCarry. E.g.
    //     // 30h - 1h -> 0011 0000
    //     //           - 0000 0001
    //     //             0010 1111 -> 2F (upper nibble affected)
    //     self.reg.set_flag(Flag::HalfCarry, (value & 0x0F) == 0);
    //     self.reg.set_flag(Flag::Negative, true);
    //     self.write_into(dst, result as u16);

    //     8
    // }

    // fn push(&mut self, reg: RegisterType16) -> u8 {
    //     let value = self.reg.reg16(reg);
    //     self.push_stack(value as u16)
    // }

    // fn push_stack(&mut self, word: u16) -> u8 {
    //     self.reg.sp -= 2;
    //     self.mmu.write_word(self.reg.sp, word);

    //     8
    // }

    // fn pop(&mut self, reg: RegisterType16) -> u8 {
    //     let value = self.mmu.word(self.reg.sp);
    //     self.reg.sp += 2;
    //     self.reg.set_reg16(reg, value);

    //     8
    // }

    // fn ret(&mut self, cond: Option<Condition>) -> u8 {
    //     let cpu_flags: FlagsRegister = self.reg.f.into();
    //     let ok = match cond {
    //         Some(cond) => cond.is_satisfied(cpu_flags),
    //         None => true,
    //     };

    //     if ok {
    //         let value = self.mmu.word(self.reg.sp) as usize;
    //         self.reg.sp += 2;
    //         self.reg.pc = value;
    //     }

    //     8
    // }

    // fn sub(&self, src: Source) -> (u8, u8) {
    //     let value = self.value_from_source(src) as u8;
    //     let result = self.reg.a == value;
    // }
    // fn cp(&self, src: Source) -> u8 {
    //     let value = self.value_from_source(src) as u8;
    //     let result = self.reg.a.wrapping_sub(value);


    //     self.reg.set_flag(Flag::Zero, result == 0);
    //     self.reg.set_flag(Flag::Negative, true);
    //     // self.reg.set_flag()
    //     self.reg.set_flag(Flag::HalfCarry, (value & 0x0F) + 1 > 0x0F);
    //     self.reg.set_flag(Flag::Carry, result < 0);

    //     8
    // }

    // fn rl(&mut self, dst: Destination) -> u8 {
    //     let value = self.value_from_destination(dst) as u8;
    //     let will_carry = value & 0x80 == 0x80; // is the MSB 1? then we will carry
    //     let result = (value << 1) | if self.reg.flag(Flag::Carry) { 1 } else { 0 };

    //     self.reg.set_flag(Flag::Zero, result == 0);
    //     self.reg.set_flag(Flag::Negative, false);
    //     self.reg.set_flag(Flag::HalfCarry, false);
    //     self.reg.set_flag(Flag::Carry, will_carry);
    //     self.write_into(dst, result as u16);

    //     8
    // }

    // fn write_into(&mut self, dst: Destination, src_value: u16) {
    //     match dst {
    //         Destination::Direct(target) => match target {
    //             Target::Register8(reg) => self.reg.set_reg8(reg, src_value as u8),
    //             Target::Register16(reg) => self.reg.set_reg16(reg, src_value),
    //             Target::Address(addr) => self.mmu.write_byte(addr, src_value as u8),
    //         },
    //         Destination::Indirect(reg) => {
    //             let addr = self.reg.reg16(reg);
    //             self.mmu.write_word(addr as usize, src_value);
    //         }
    //         Destination::Indexed(target, addr) => match target {
    //             IndexedTarget::Register8(reg) => {
    //                 let addr_offset = self.reg.reg8(reg);
    //                 self.mmu
    //                     .write_byte(addr as usize + addr_offset as usize, src_value as u8);
    //             }
    //             IndexedTarget::Immediate8(addr_offset) => {
    //                 self.mmu
    //                     .write_byte(addr as usize + addr_offset as usize, src_value as u8);
    //             }
    //         },
    //     }
    // }

    // fn value_from_destination(&self, dst: Destination) -> u16 {
    //     match dst {
    //         Destination::Direct(target) => self.direct_from_target(target),
    //         Destination::Indirect(target) => self.indirect_from_target(target) as u16,
    //         Destination::Indexed(target, offset) => self.indexed_from_target(target, offset) as u16,
    //     }
    // }

    // fn value_from_source(&self, src: Source) -> u16 {
    //     match src {
    //         Source::Immediate8(n) => n as u16,
    //         Source::Immediate16(nn) => nn,
    //         Source::Direct(target) => self.direct_from_target(target),
    //         Source::Indirect(target) => self.indirect_from_target(target) as u16,
    //         Source::Indexed(target, offset) => self.indexed_from_target(target, offset) as u16,
    //         Source::Offset(reg, offset) => {
    //             let v = self.reg.reg16(reg);
    //             v.wrapping_add(offset as u16)
    //         }
    //     }
    // }

    // fn direct_from_target(&self, target: Target) -> u16 {
    //     match target {
    //         Target::Register8(reg) => self.reg.reg8(reg) as u16,
    //         Target::Register16(reg) => self.reg.reg16(reg),
    //         Target::Address(addr) => addr as u16,
    //     }
    // }

    // fn indirect_from_target(&self, reg: RegisterType16) -> u8 {
    //     let addr = self.reg.reg16(reg);
    //     self.mmu.byte(addr as usize)
    // }

    // fn indexed_from_target(&self, target: IndexedTarget, offset_addr: u16) -> u8 {
    //     match target {
    //         IndexedTarget::Register8(reg) => {
    //             let offset = self.reg.reg8(reg);
    //             self.mmu.byte(offset as usize + offset_addr as usize)
    //         }
    //         IndexedTarget::Immediate8(n) => self.mmu.byte(n + offset_addr as usize),
    //     }
    // }
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

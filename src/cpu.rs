use crate::mmu::Mmu;
use crate::error::GBError;

#[derive(Debug)]
pub enum ProgramCounter {
    Next,
    Jump(usize)
}

#[derive(Debug)]
pub enum Operand {
    Addr(usize),
    Byte(u8),
    Register(Register),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum Register {
    V(usize),
    I,
    Dt,
    St,
    Pc,
    Sp,
}

#[derive(Debug)]
pub enum Op {
    CLS,                    // Clear
    RET,                    // Return
    JP(usize),              // Jump
    JPREG(Operand, usize),  // Jump
    CALL(usize),
    SE(Register, Operand),  // Skip next if eq
    SNE(Register, Operand), // Skip next if not eq
    LD(Operand, Operand),

    ADD(Register, Operand),
    OR(Register, Operand),
    AND(Register, Operand),
    XOR(Register, Operand),
    SUB(Register, Operand),
    SHR(Register, Operand),
    SUBN(Register, Operand),
    SHL(Register, Operand),
    RND(Register, u8),

    DRAW(Register, Register, usize),
    SKP(Operand),
    SKNP(Operand),

    SKIPKEY(Register),
    SKIPNOKEY(Register),
    WAITKEY(Register),
    SPRITECHAR(Register),
    MOVBCD(Register),
    READM(Register),
    WRITEM(Register),
}


pub struct Cpu {
    mmu: Mmu,
    // general purpose registers
    v: [u8; 16],

    // address store register
    i: usize,

    stack: [usize; 16],
    pub pc: usize,
    sp: usize,
}

impl Cpu {
    pub fn new(mmu: Mmu) -> Cpu {
        Cpu {
            mmu,
            v: [0u8; 16],
            i: 0,
            stack: [0usize; 16],
            pc: 0x200,
            sp: 0,
        }
    }

    pub fn read_instruction(&mut self) -> Result<Op, GBError> {
        let op = self.mmu.read_word(self.pc)?;
        let nibbles = (
            (op & 0xF000) >> 12,
            (op & 0x0F00) >> 8,
            (op & 0x00F0) >> 4,
            op & 0x000F,
        );
        let nnn: usize = (op & 0x0FFF) as usize;
        let kk = (op & 0x00FF) as u8;
        let x = nibbles.1 as usize;
        let y = nibbles.2 as usize;
        let n = nibbles.3 as usize;

        match nibbles {
            (0x0, 0x0, 0xE, 0x0) => Ok(Op::CLS),
            (0x0, 0x0, 0xE, 0xE) => Ok(Op::RET),
            (0x1, _, _, _) => Ok(Op::JP(nnn)),
            (0x2, _, _, _) => Ok(Op::CALL(nnn)),
            (0x3, _, _, _) => Ok(Op::SE(Register::V(x), Operand::Byte(kk))),
            (0x4, _, _, _) => Ok(Op::SNE(Register::V(x), Operand::Byte(kk))),
            (0x5, _, _, 0x0) => Ok(Op::SE(Register::V(x), Operand::Register(Register::V(y)))),
            (0x6, _, _, _) => Ok(Op::LD(Operand::Register(Register::V(x)), Operand::Byte(kk))),
            (0x7, _, _, _) => Ok(Op::ADD(Register::V(x), Operand::Byte(kk))),

            (0x8, _, _, 0x0) => Ok(Op::LD(Operand::Register(Register::V(x)), Operand::Register(Register::V(y)))),
            (0x8, _, _, 0x1) => Ok(Op::OR(Register::V(x), Operand::Register(Register::V(y)))),
            (0x8, _, _, 0x2) => Ok(Op::AND(Register::V(x), Operand::Register(Register::V(y)))),
            (0x8, _, _, 0x3) => Ok(Op::XOR(Register::V(x), Operand::Register(Register::V(y)))),
            (0x8, _, _, 0x4) => Ok(Op::ADD(Register::V(x), Operand::Register(Register::V(y)))),
            (0x8, _, _, 0x5) => Ok(Op::SUB(Register::V(x), Operand::Register(Register::V(y)))),
            (0x8, _, _, 0x6) => Ok(Op::SHR(Register::V(x), Operand::Register(Register::V(y)))),
            (0x8, _, _, 0x7) => Ok(Op::SUBN(Register::V(x), Operand::Register(Register::V(y)))),
            (0x8, _, _, 0xE) => Ok(Op::SHL(Register::V(x), Operand::Register(Register::V(y)))),

            (0x9, _, _, 0x0) => Ok(Op::SNE(Register::V(x), Operand::Register(Register::V(y)))),
            (0xA, _, _, _) => Ok(Op::LD(Operand::Register(Register::I), Operand::Addr(nnn))),
            (0xB, _, _, _) => Ok(Op::JPREG(Operand::Register(Register::V(0)), nnn)),
            (0xC, _, _, _) => Ok(Op::RND(Register::V(x), kk)),
            (0xD, _, _, _) => Ok(Op::DRAW(Register::V(x), Register::V(y), n)),

            (0xE, _, 0x9, 0xE) => Ok(Op::SKIPKEY(Register::V(x))),
            (0xE, _, 0xA, 0x1) => Ok(Op::SKIPNOKEY(Register::V(x))),

            (0xF, _, _, 0x7) => Ok(Op::LD(Operand::Register(Register::V(x)), Operand::Register(Register::Dt))),
            (0xF, _, _, 0xA) => Ok(Op::WAITKEY(Register::V(x))),
            (0xF, _, 0x1, 0x5) => Ok(Op::LD(Operand::Register(Register::Dt), Operand::Register(Register::V(x)))),
            (0xF, _, 0x1, 0x8) => Ok(Op::LD(Operand::Register(Register::St), Operand::Register(Register::V(x)))),
            (0xF, _, 0x1, 0xE) => Ok(Op::ADD(Register::I, Operand::Register(Register::V(x)))),
            (0xF, _, 0x2, 0x9) => Ok(Op::SPRITECHAR(Register::V(x))),
            (0xF, _, 0x3, 0x3) => Ok(Op::MOVBCD(Register::V(x))),
            (0xF, _, 0x5, 0x5) => Ok(Op::READM(Register::V(x))),
            (0xF, _, 0x6, 0x5) => Ok(Op::WRITEM(Register::V(x))),
            _ => Err(GBError::UnknownOperation(op))
        }
    }

    pub fn execute_instruction(&mut self, instruction: Op) {
        let pc_change = match instruction {
            Op::CLS => self.clear_vram(),
            // Op::RET => {}
            Op::JP(dst) => {
                ProgramCounter::Jump(dst as usize)
            }
            // Op::JPREG(_, _) => {}
            // Op::CALL(_) => {}
            // Op::SE(_, _) => {}
            // Op::SNE(_, _) => {}
            Op::LD(dst, src) => {
                if let Operand::Register(dst) = dst {
                    if let Operand::Byte(src) = src {
                        match dst {
                            Register::V(dst) => { self.v[dst] = src; }
                            _ => panic!("can only load byte int Vx register"),
                        };
                    }
                    if let Operand::Addr(src) = src {
                        match dst {
                            Register::I => { self.i = src as usize }
                            _ => panic!("cannot load address unless dst is i"),
                        }
                    }
                }

                ProgramCounter::Next
            }
            // Op::ADD(_, _) => {}
            // Op::OR(_, _) => {}
            // Op::AND(_, _) => {}
            // Op::XOR(_, _) => {}
            // Op::SUB(_, _) => {}
            // Op::SHR(_, _) => {}
            // Op::SUBN(_, _) => {}
            // Op::SHL(_, _) => {}
            // Op::RND(_, _) => {}
            // Op::DRAW(_, _, _) => {}
            // Op::SKP(_) => {}
            // Op::SKNP(_) => {}
            // Op::SKIPKEY(_) => {}
            // Op::SKIPNOKEY(_) => {}
            // Op::WAITKEY(_) => {}
            // Op::SPRITECHAR(_) => {}
            // Op::MOVBCD(_) => {}
            // Op::READM(_) => {}
            // Op::WRITEM(_) => {}
            _ => ProgramCounter::Next,
        };

        match pc_change {
            ProgramCounter::Next => { self.pc += 2 }
            ProgramCounter::Jump(addr) => { self.pc = addr }
        }
    }

    fn clear_vram(&mut self) -> ProgramCounter {
        self.mmu.clear_vram();
        ProgramCounter::Next
    }
}

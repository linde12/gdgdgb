use std::env;
use std::fs::File;
use std::io::prelude::*;

use anyhow::{self, Context};
use thiserror::Error;

#[derive(Debug, Error)]
enum GBError {
    #[error("Unknown operation {:?}", .0)]
    UnknownOperation(u8),

    #[error("Unable to read byte")]
    ReadByte,

    #[error("Unable to read word")]
    ReadWord,

    #[error("Unable to load rom: {}", .0)]
    LoadRom(String),

    #[error("Bad command")]
    BadCommand,
}

#[derive(Debug)]
enum ProgramCounter {
    Next,
    Jump(usize),
}

#[derive(Debug)]
enum Operand {
    Addr(usize),
    AddrRef(Register),
    Byte(u8),
    Word(u16),
    Register(Register),
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
}

#[derive(Debug)]
enum Op {
    NOP,
    LD(Operand, Operand),
}

struct Mmu {
    ram: Vec<u8>,
    vram: Vec<u8>,
}

impl Mmu {
    fn new() -> Mmu {
        Mmu {
            ram: vec![0; 4096],
            vram: vec![0; 2048],
        }
    }

    fn read_byte(&self, index: usize) -> Result<u8, GBError> {
        if index > self.ram.len() {
            Err(GBError::ReadByte)
        } else {
            Ok(self.ram[index])
        }
    }

    fn read_word(&self, index: usize) -> Result<u16, GBError> {
        if index + 1 > self.ram.len() {
            Err(GBError::ReadWord)
        } else {
            Ok(((self.ram[index] as u16) << 8) + (self.ram[index + 1] as u16))
        }
    }

    fn load_rom(&mut self, rom: Vec<u8>) -> Result<(), GBError> {
        if rom.len() > 4096 {
            return Err(GBError::LoadRom("rom size too large".into()));
        }
        // self.mem.clone_from_slice(&rom);
        for (i, item) in rom.iter().enumerate() {
            self.ram.insert(i + 0x200, *item);
        }
        // copy(&mut self.mem[..], &mut rom);
        // self.mem = rom;
        Ok(())
    }

    fn clear_vram(&mut self) {
        self.vram.clear();
    }
}

struct Cpu {
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
    pc: usize,
    sp: usize,
}

impl Cpu {
    fn new(mmu: Mmu) -> Cpu {
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
            pc: 0x200,
            sp: 0,
        }
    }

    fn read_hl(&self) -> usize {
        (((self.h as u16) << 8) + self.l as u16) as usize
    }

    fn write_hl(&mut self, value: u16) {
        self.l = (value & 0x0F) as u8;
        self.h = ((value & 0xF0) >> 8) as u8;
    }

    fn read_instruction(&mut self) -> Result<Op, GBError> {
        let op = self.mmu.read_byte(self.pc)?;
        let high = op & 0xF0;
        let low = op & 0x0F;

        match op {
            0x00 => Ok(Op::NOP),
            0x01 => {
                let value = self.mmu.read_word(self.pc)?;
                Ok(Op::LD(
                    Operand::Register(Register::BC),
                    Operand::Word(value),
                ))
            }

            0x06 => {
                let value = self.mmu.read_byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::B), Operand::Byte(value)))
            }

            0x0E => {
                let value = self.mmu.read_byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::C), Operand::Byte(value)))
            }

            0x16 => {
                let value = self.mmu.read_byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::D), Operand::Byte(value)))
            }

            0x1E => {
                let value = self.mmu.read_byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::E), Operand::Byte(value)))
            }

            0x26 => {
                let value = self.mmu.read_byte(self.pc)?;
                Ok(Op::LD(Operand::Register(Register::H), Operand::Byte(value)))
            }

            0x2E => {
                let value = self.mmu.read_byte(self.pc)?;
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
            0x7E => {
                let value = self.mmu.read_word(self.read_hl() as usize)?;
                Ok(Op::LD(Operand::Register(Register::A), Operand::Word(value)))
            }

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

            0x46 => {
                let value = self.mmu.read_word(self.read_hl() as usize)?;
                Ok(Op::LD(Operand::Register(Register::B), Operand::Word(value)))
            }

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
            0x4E => {
                let value = self.mmu.read_word(self.read_hl() as usize)?;
                Ok(Op::LD(Operand::Register(Register::C), Operand::Word(value)))
            }

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

            0x56 => {
                let value = self.mmu.read_word(self.read_hl() as usize)?;
                Ok(Op::LD(Operand::Register(Register::D), Operand::Word(value)))
            }

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
            0x5E => {
                let value = self.mmu.read_word(self.read_hl() as usize)?;
                Ok(Op::LD(Operand::Register(Register::E), Operand::Word(value)))
            }

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

            0x66 => {
                let value = self.mmu.read_word(self.read_hl() as usize)?;
                Ok(Op::LD(Operand::Register(Register::H), Operand::Word(value)))
            }

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

            0x6e => {
                let value = self.mmu.read_word(self.read_hl() as usize)?;
                Ok(Op::LD(Operand::Register(Register::L), Operand::Word(value)))
            }

            // LD (HL), x
            0x70 => Ok(Op::LD(
                Operand::AddrRef(Register::HL),
                Operand::Register(Register::B),
            )),

            0x71 => Ok(Op::LD(
                Operand::AddrRef(Register::HL),
                Operand::Register(Register::C),
            )),

            0x72 => Ok(Op::LD(
                Operand::AddrRef(Register::HL),
                Operand::Register(Register::D),
            )),

            0x73 => Ok(Op::LD(
                Operand::AddrRef(Register::HL),
                Operand::Register(Register::E),
            )),

            0x74 => Ok(Op::LD(
                Operand::AddrRef(Register::HL),
                Operand::Register(Register::H),
            )),

            0x75 => Ok(Op::LD(
                Operand::AddrRef(Register::HL),
                Operand::Register(Register::L),
            )),

            _ => Err(GBError::UnknownOperation(op)),
        }
    }

    fn execute_instruction(&mut self, instruction: Op) {}

    fn clear_vram(&mut self) -> ProgramCounter {
        self.mmu.clear_vram();
        ProgramCounter::Next
    }
}

fn main() -> anyhow::Result<()> {
    let cmd = env::args().nth(1).context("no command provided")?;
    let fp = env::args().nth(2).context("no rom provided")?;
    let mut file = File::open(fp).context("unable to open rom")?;
    let mut rom: Vec<u8> = Vec::with_capacity(100);
    file.read_to_end(&mut rom).context("unable to read rom")?;

    let mut mmu = Mmu::new();
    mmu.load_rom(rom)?;
    let mut cpu = Cpu::new(mmu);

    match cmd.as_str() {
        "d" | "disassemble" | "disasm" => loop {
            let op = cpu.read_instruction()?;
            println!("{:#x}\t{:?}", cpu.pc, op);
            cpu.pc += 2;
        },
        "r" | "run" => loop {
            let op = cpu.read_instruction()?;
            cpu.execute_instruction(op);
        },
        _ => Err(GBError::BadCommand.into()),
    }
}

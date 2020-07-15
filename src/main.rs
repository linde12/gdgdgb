use std::env;
use std::fs::File;
use std::io::prelude::*;
use anyhow::{self, Context};

mod mmu;
mod cpu;
mod error;

use crate::mmu::Mmu;
use crate::cpu::Cpu;
use error::GBError;

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
        }
        _ => Err(GBError::BadCommand.into()),
    }
}

use anyhow::{self, Context};
use sdl_display::SdlDisplay;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::thread::sleep;
use std::time::{Duration, Instant};

mod cpu;
mod display;
mod error;
mod mmu;
mod ppu;
mod sdl_display;

use crate::cpu::Cpu;
use crate::display::Display;
use crate::mmu::Mmu;
use crate::ppu::{PPU, SCALE, XRES, YRES};
use error::GBError;

fn main() -> anyhow::Result<()> {
    let cmd = env::args().nth(1).context("no command provided")?;
    let fp = env::args().nth(2).context("no rom provided")?;
    let mut file = File::open(fp).context("unable to open rom")?;
    let mut rom: Vec<u8> = Vec::with_capacity(100);
    file.read_to_end(&mut rom).context("unable to read rom")?;

    let mut mmu = Mmu::new();
    mmu.load_rom(rom);
    let mut cpu = Cpu::new();

    let ctx = sdl2::init().unwrap();
    let mut event_pump = ctx.event_pump().unwrap();
    let video_subsystem = ctx.video().unwrap();
    let window = video_subsystem
        .window("gdgdgb", (XRES * SCALE) as u32, (YRES * SCALE) as u32)
        .position_centered()
        .build()
        .unwrap();
    let canvas = window.into_canvas().build().unwrap();
    let display = SdlDisplay::new(canvas);
    let mut ppu = PPU::new(&mut mmu, display);

    // let breakpoints = 0x0150..=0x0150;
    let breakpoints = vec![0x00fe];
    // let breakpoints = vec![0x0066, 0x0068, 0x006a];
    //
    let mut last_frame = Instant::now();
    let fps_target = Duration::from_micros(16_740); // ~59.7 fps

    match cmd.as_str() {
        "d" | "disassemble" | "disasm" => loop {
            let pc = cpu.pc;
            let op = cpu.read_instruction(&mut mmu)?;
            if pc == 0x0003 {
                cpu.print_state();
                return Ok(());
            }
            println!("0x{:04X}\t{:02X?}", pc, op);
            // cpu.execute_instruction(op, &mmu);
        },
        "r" | "run" => loop {
            let pc = cpu.pc;
            if breakpoints.contains(&pc) {
                cpu.print_state();
                println!("LCDC = 0x{:02X}", mmu.byte(0xFF40));
                std::io::stdin().read_line(&mut String::new())?;
            }
            let op = cpu.read_instruction(&mut mmu)?;
            let consumed_cycles = cpu.execute_instruction(op, &mut mmu);
            ppu.step(consumed_cycles, &mut mmu);

            if ppu.take_frame_ready() {
                for event in event_pump.poll_iter() {
                    match event {
                        sdl2::event::Event::Quit { .. } => return Ok(()),
                        _ => {}
                    }
                }
                let elapsed = last_frame.elapsed();
                if elapsed < fps_target {
                    sleep(fps_target - elapsed);
                }
                last_frame = Instant::now();
            }
        },
        _ => Err(GBError::BadCommand.into()),
    }
}

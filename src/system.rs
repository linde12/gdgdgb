use crate::error::GBError;
use crate::{Cpu, Debugger, Mmu};
use std::io::stdin;
use structopt::StructOpt;

pub struct System {
    cpu: Cpu,
    debugger: Option<Debugger>,
}

impl System {
    pub fn new(cpu: Cpu, debugger: Option<Debugger>) -> Self {
        Self { cpu, debugger }
    }

    pub fn run(&mut self) -> Result<(), GBError> {
        match &mut self.debugger {
            Some(debugger) => {
                let pc = self.cpu.pc();

                if debugger.should_break(pc) {
                    let mut buf = String::new();
                    stdin().read_line(&mut buf).expect("no stdin");
                    debugger.handle_command(buf);
                }

                let op = self.cpu.read_instruction()?;
                println!("0x{:04X}\t{:02X?}", pc, op);
                self.cpu.execute_instruction(op);
            },
            None => {
                let op = self.cpu.read_instruction()?;
                self.cpu.execute_instruction(op);
            },
        }

        Ok(())
    }
}

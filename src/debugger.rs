use std::collections::HashMap;
use crate::System;
use structopt::StructOpt;

use lazy_static::lazy_static;

fn parse_addr(s: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(s, 16)
}

#[derive(StructOpt, Debug)]
#[structopt(name = "break", about = "Handle breakpoints")]
pub enum BreakCommand {
    #[structopt(name = "add")]
    Add {
        #[structopt(name = "addr", parse(try_from_str = "parse_addr"))]
        addr: u16,
    },
    #[structopt(name = "remove")]
    Remove {
        #[structopt(name = "addr", parse(try_from_str = "parse_addr"))]
        addr: u16
    },

    #[structopt(name = "list")]
    List,
}

impl CommandHandler for BreakCommand {
    fn handle(&self, debugger: &mut Debugger) {
        match self {
            BreakCommand::Add { addr } => {
                debugger.breakpoints.insert(*addr as usize, ());
            }
            BreakCommand::Remove { addr } => {
                let word = *addr as usize;
                debugger.breakpoints.remove(&word);
            }
            BreakCommand::List => {}
        }
        // debugger.breakpoints.push()
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "dump", about = "Dump information")]
pub enum DumpCommand {
    #[structopt(name = "flags")]
    Flags,
    #[structopt(name = "regs")]
    Registers,
}

#[derive(StructOpt, Debug)]
#[structopt(name = "step", about = "Step to next instruction")]
pub struct StepCommand {}

#[derive(StructOpt, Debug)]
#[structopt(name = "cont", about = "Continue execution until next breakpoint")]
pub struct ContinueCommand {}

#[derive(StructOpt, Debug)]
#[structopt(name = "quit", about = "Quit program")]
pub struct QuitCommand {}

#[derive(Default)]
pub struct Debugger {
    breakpoints: HashMap<usize, ()>,
}

pub trait CommandHandler: Sized {
    fn handle(&self, debugger: &mut Debugger);
}

struct Command {
    name: &'static str,
    handler: Box<dyn Fn(&mut Debugger) + Sync>,
}

impl Command {
    fn new(name: &'static str, handler: Box<dyn Fn(&mut Debugger) + Sync>) -> Self {
        Command { name, handler }
    }
}

impl Debugger {
    pub fn new() -> Self {
        let mut breakpoints = HashMap::new();
        breakpoints.insert(0x0000, ());
        Self {
            breakpoints,
            ..Default::default()
        }
    }

    pub fn should_break(&self, addr: usize) -> bool {
        self.breakpoints.contains_key(&addr)
    }

    pub fn handle_command(&mut self, line: String) {
        let mut commands = line.split_whitespace();
        let cmd = match commands.next() {
            Some(cmd) => cmd,
            None => "",
        };

        let command = match cmd {
            "break" => Some(BreakCommand::from_iter_safe(commands)),
            _ => None,
        };

        if let Some(command) = command {
            match command {
                Ok(cmd) => {
                    cmd.handle(self);
                },
                Err(err) => {
                    eprintln!("{}", err);
                    // TODO: Return bool to caller and loop stdin if result is false (e.g. typo)
                    std::process::exit(1);
                }
            };
        }
    }
}


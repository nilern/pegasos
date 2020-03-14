extern crate rustyline;

use rustyline::error::ReadlineError;
use std::fs;
use std::io::{stderr, Write};
use std::path::PathBuf;
use std::process::exit;
use structopt::StructOpt;

mod error;
mod gc;
mod interpreter;
mod lexer;
mod objects;
mod parser;
mod primitives;
mod refs;
mod state;
mod util;

use interpreter::run;
use lexer::Lexer;
use parser::Parser;
use state::State;

const PROMPT: &str = "pegasos> ";

#[derive(StructOpt, Debug)]
#[structopt(name = "pegasos")]
struct CliArgs {
    #[structopt(short, long)]
    debug: bool,

    /// Set include path
    #[structopt(short = "I", parse(from_os_str))]
    path: Vec<PathBuf>,

    /// Files to `(load)` initially
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>
}

fn main() {
    let CliArgs { debug, path, files } = CliArgs::from_args();

    let mut state = State::new(&path, 1 << 18, 1 << 20);
    let mut editor = rustyline::Editor::<()>::new();

    for path in files {
        let contents = fs::read_to_string(&path).unwrap_or_else(|err| {
            writeln!(stderr(), "IO Error reading {}: {}", path.display(), err).unwrap();
            exit(1);
        });
        let mut parser = Parser::new(Lexer::new(contents.chars()));

        match unsafe { parser.sexprs(&mut state, path.to_str().unwrap()) } {
            Ok(()) => match run(&mut state) {
                Ok(()) => {},
                Err(err) => {
                    writeln!(stderr(), "Error loading {}: {}", path.display(), err).unwrap();
                    exit(1);
                }
            },
            Err(err) => {
                writeln!(stderr(), "Error reading {}: {}", path.display(), err).unwrap();
                exit(1);
            }
        }
    }

    loop {
        match editor.readline(PROMPT) {
            Ok(line) => {
                editor.add_history_entry(line.as_str());

                state.unwind();
                let mut parser = Parser::new(Lexer::new(line.chars()));
                match unsafe { parser.sexprs(&mut state, "REPL") } {
                    Ok(()) => match run(&mut state) {
                        Ok(()) => println!("Ack, result: {}", state.pop().unwrap()),
                        Err(err) => {
                            println!("Runtime error: {}", err);

                            if debug {
                                println!("");
                                unsafe { state.dump(&mut stderr()).unwrap() };
                                println!("\n");
                            }
                        }
                    },
                    Err(err) => println!("Parse error: {}", err)
                }
            },
            Err(ReadlineError::Interrupted) => {
                // NOTE: Not that useful while interpreter does not handle signals
                println!("Ack, stopping.");
            },
            Err(ReadlineError::Eof) => {
                println!("Ack, quitting.");
                break;
            },
            Err(err) => {
                println!("Readline error: {:?}", err);
                break;
            }
        }
    }
}

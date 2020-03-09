extern crate rustyline;

use std::fs::File;
use std::io::{Read, Write, stderr};
use std::path::PathBuf;
use std::process::exit;
use structopt::StructOpt;
use rustyline::error::ReadlineError;

mod util;
mod gc;
mod refs;
mod objects;
mod lexer;
mod state;
mod parser;
mod interpreter;
mod error;

use lexer::Lexer;
use parser::Parser;
use state::State;
use interpreter::eval;

const PROMPT: &str = "pegasos> ";

#[derive(StructOpt, Debug)]
#[structopt(name = "pegasos")]
struct CliArgs {
    #[structopt(short, long)]
    debug: bool,
    
    /// Files to `(load)` initially
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>
}

fn main() {
    let CliArgs {debug, files} = CliArgs::from_args();

    let mut state = State::new(1 << 16, 1 << 20);
    let mut editor = rustyline::Editor::<()>::new();

    for path in files {
        match File::open(&path) {
            Ok(mut file) => {
                let mut contents = String::new();
                file.read_to_string(&mut contents).unwrap_or_else(|err| {
                    writeln!(stderr(), "IO Error reading {}: {}", path.display(), err).unwrap();
                    exit(1);
                });
                let mut parser = Parser::new(Lexer::new(contents.chars()));

                loop {
                    match unsafe { parser.sexpr(&mut state) } {
                        Ok(Some(())) => match eval(&mut state) {
                            Ok(()) => {},
                            Err(err) => {
                                writeln!(stderr(), "Error loading {}: {}", path.display(), err).unwrap();
                                exit(1);
                            }
                        },
                        Ok(None) => break,
                        Err(err) => {
                            writeln!(stderr(), "Error loading {}: {}", path.display(), err).unwrap();
                            exit(1);
                        }
                    }
                }
            },
            Err(err) => {
                writeln!(stderr(), "IO Error opening {}: {}", path.display(), err).unwrap();
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
                match unsafe { parser.sexpr(&mut state) } {
                    Ok(Some(())) => match eval(&mut state) {
                        Ok(()) => println!("Ack, result: {}", state.pop().unwrap()),
                        Err(err) => {
                            println!("Runtime error: {}", err);

                            if debug {
                                println!("");
                                unsafe { state.dump(&mut stderr()).unwrap(); }
                                println!("\n");
                            }
                        }
                    },
                    Ok(None) => {},
                    Err(err) => println!("Parse error: {}", err)
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("Ack, stopping.");
                // TODO: Enable interrupting evaluation (e.g. infinite loops).
            },
            Err(ReadlineError::Eof) => {
                println!("Ack, quitting.");
                break;
            }
            Err(err) => {
                println!("Readline error: {:?}", err);
                break;
            }
        }
    }
}

extern crate rustyline;

use std::io::stderr;
use structopt::StructOpt;
use rustyline::error::ReadlineError;

mod util;
mod gc;
mod value;
mod lexer;
mod state;
mod parser;
mod bindings;
mod interpreter;

use lexer::Lexer;
use parser::Parser;
use state::State;
use interpreter::eval;

const PROMPT: &str = "pegasos> ";

#[derive(StructOpt, Debug)]
#[structopt(name = "pegasos")]
struct CliArgs {
    #[structopt(short, long)]
    debug: bool
}

fn main() {
    let CliArgs {debug} = CliArgs::from_args();

    let mut state = State::new(1 << 16, 1 << 20);
    let mut editor = rustyline::Editor::<()>::new();

    loop {
        match editor.readline(PROMPT) {
            Ok(line) => {
                editor.add_history_entry(line.as_str());

                state.unwind();
                let mut parser = Parser::new(Lexer::new(&line).peekable());
                match parser.sexpr(&mut state) {
                    Ok(()) => match eval(&mut state) {
                        Ok(()) => println!("Ack, result: {}", state.pop().unwrap()),
                        Err(()) => {
                            println!("Runtime error.");

                            if debug {
                                println!("");
                                unsafe { state.dump(&mut stderr()).unwrap(); }
                                println!("\n");
                            }
                        }
                    },
                    Err(()) => println!("Parse error.")
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

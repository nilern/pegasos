extern crate rustyline;

use std::io::stderr;
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
                    Some(Ok(())) => match eval(&mut state) {
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
                    Some(Err(err)) => println!("Parse error: {}", err),
                    None => {}
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

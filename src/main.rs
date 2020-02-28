extern crate rustyline;

use rustyline::error::ReadlineError;

mod util;
mod gc;
mod value;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::parse_expr;

const PROMPT: &str = "pegasos> ";

fn main() {
    let mut editor = rustyline::Editor::<()>::new();

    loop {
        match editor.readline(PROMPT) {
            Ok(line) => {
                editor.add_history_entry(line.as_str());

                let mut lexer = Lexer::new(&line).peekable();
                match parse_expr(&mut lexer) {
                    Some(v) => println!("Ack, echo: {}", v),
                    None => println!("Parse error.")
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

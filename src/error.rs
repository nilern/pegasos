use std::fmt::{self, Display, Formatter};

use super::parser;
use super::interpreter::{SyntaxError, RuntimeError};

#[derive(Debug)]
pub enum PgsError {
    Parse(parser::Error),
    Syntax(SyntaxError),
    Runtime(RuntimeError)
}

impl From<parser::Error> for PgsError {
    fn from(err: parser::Error) -> Self { PgsError::Parse(err) }
}

impl From<SyntaxError> for PgsError {
    fn from(err: SyntaxError) -> Self { PgsError::Syntax(err) }
}

impl From<RuntimeError> for PgsError {
    fn from(err: RuntimeError) -> Self { PgsError::Runtime(err) }
}

impl Display for PgsError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            PgsError::Parse(parse_err) => write!(f, "Parse error: {}", parse_err),
            PgsError::Syntax(syn_err) => write!(f, "Syntax error: {}", syn_err),
            PgsError::Runtime(rt_err) => write!(f, "Runtime error: {}", rt_err)
        }
    }
}


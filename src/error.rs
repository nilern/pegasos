use std::fmt::{self, Display, Formatter};

use super::parser;
use super::interpreter::{SyntaxError, RuntimeError};

#[derive(Debug)]
pub enum PgsError<'a> {
    Parse(parser::Error<'a>),
    Syntax(SyntaxError),
    Runtime(RuntimeError)
}

impl<'a> From<parser::Error<'a>> for PgsError<'a> {
    fn from(err: parser::Error<'a>) -> Self { PgsError::Parse(err) }
}

impl<'a> From<SyntaxError> for PgsError<'a> {
    fn from(err: SyntaxError) -> Self { PgsError::Syntax(err) }
}

impl<'a> From<RuntimeError> for PgsError<'a> {
    fn from(err: RuntimeError) -> Self { PgsError::Runtime(err) }
}

impl<'a> Display for PgsError<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            PgsError::Parse(parse_err) => write!(f, "Parse error: {}", parse_err),
            PgsError::Syntax(syn_err) => write!(f, "Syntax error: {}", syn_err),
            PgsError::Runtime(rt_err) => write!(f, "Runtime error: {}", rt_err)
        }
    }
}


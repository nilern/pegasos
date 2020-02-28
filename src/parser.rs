use std::iter::Peekable;

use super::lexer::{Token, Lexer};
use super::value::Value;

fn parse_expr(lexer: &mut Peekable<Lexer>) -> Option<Value> {
    use Token::*;

    match lexer.peek() {
        Some(Const(_)) => {
            if let Some(Const(v)) = lexer.next() {
                Some(v)
            } else {
                unreachable!()
            }
        },
        Some(_) => unimplemented!(),
        None => None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryFrom;

    #[test]
    fn test_const() {
        let mut lexer = Lexer::new(" 23 ").peekable();
        assert_eq!(parse_expr(&mut lexer), Some(Value::try_from(23).unwrap()));
    }
}


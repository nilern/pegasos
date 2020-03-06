use std::convert::TryFrom;
use std::str::Chars;

use super::value::Value;

// ---

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    LParen, RParen, OpenVector,
    Dot,
    Quote,
    Identifier(&'a str),
    Const(Value)
}

// ---

#[derive(Clone, Copy)]
enum Radix {
    Binary = 0b10,
    Octal = 0o10,
    Decimal = 10,
    Hexadecimal = 0x10
}

// ---

pub struct Lexer<'a> {
    chars: Chars<'a>
}

impl<'a> Lexer<'a> {
    pub fn new(chars: &'a str) -> Self { Self {chars: chars.chars()} }

    fn peek(&self) -> Option<char> { self.chars.as_str().chars().next() }

    fn boolean(&mut self) -> Option<Token<'a>> {
        match self.chars.next() {
            Some('t') => return Some(Token::Const(Value::TRUE)),
            Some('f') => return Some(Token::Const(Value::FALSE)),
            _ => unreachable!()
        }
    }

    fn number(&mut self, radix: Radix) -> Option<Token<'a>> {
        let mut res: Option<isize> = None;

        loop {
            match self.peek() {
                Some(c) if c.is_digit(radix as u32) => {
                    let _ = self.chars.next();
                    let m = c.to_digit(radix as u32).unwrap() as isize;
                    res = Some(match res {
                        Some(n) =>
                            n.checked_mul(radix as isize).unwrap()
                                .checked_add(m).unwrap(),
                        None => m
                    });
                },
                _ => return res.map(|n| Token::Const(Value::try_from(n).unwrap()))
            }
        }
    }

    fn identifier(&mut self) -> Option<Token<'a>> {
        let cs = self.chars.as_str();
        let mut len = 0;

        loop {
            match self.peek() {
                Some(c) if c.is_alphabetic() || c == '!' => {
                    let _ = self.chars.next();
                    len += 1;
                },
                _ => return Some(Token::Identifier(&cs[0..len]))
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;

        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => {
                    let _ = self.chars.next();
                },
                Some('(') => {
                    let _ = self.chars.next();
                    return Some(LParen);
                },
                Some(')') => {
                    let _ = self.chars.next();
                    return Some(RParen);
                },
                Some('.') => {
                    let _ = self.chars.next();
                    return Some(Dot);
                },
                Some('\'') => {
                    let _ = self.chars.next();
                    return Some(Quote);
                },
                Some('#') => {
                    let _ = self.chars.next();
                    match self.peek() {
                        Some('t') => return self.boolean(),
                        Some('f') => return self.boolean(),
                        Some('(') => {
                            let _ = self.chars.next();
                            return Some(OpenVector);
                        }
                        _ => unimplemented!()
                    }
                },
                Some(c) if c.is_digit(10) => return self.number(Radix::Decimal),
                Some(c) if c.is_alphabetic() || c == '!' => return self.identifier(),
                None => return None,
                _ => unimplemented!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        use Token::*;

        let input = "  (23 #f foo)  ";
        let chars: Vec<Token> = Lexer::new(&input).collect();
        assert_eq!(chars, vec![LParen, Const(Value::try_from(23isize).unwrap()), Const(Value::FALSE), Identifier("foo"), RParen]);
    }
}


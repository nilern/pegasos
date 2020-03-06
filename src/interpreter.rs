use std::convert::{TryFrom, TryInto};
use std::mem::transmute;

use super::state::State;
use super::value::{Value, UnpackedValue, UnpackedHeapValue, PgsString, Symbol, Pair};

enum Op {Eval, Continue}

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
pub enum FrameTag {
    Done = 0 << Value::SHIFT,
    CondBranch = 1 << Value::SHIFT,
    Define = 2 << Value::SHIFT,
    Set = 3 << Value::SHIFT
}

impl FrameTag {
    pub fn framesize(self) -> usize {
        use FrameTag::*;

        match self {
            Done => 0,
            CondBranch => 2,
            Define => 1,
            Set => 1
        }
    }
}

impl From<FrameTag> for Value {
    fn from(tag: FrameTag) -> Value { unsafe { transmute(tag) } }
}

pub fn eval(state: &mut State) -> Result<(), ()> {
    let mut op = Op::Eval;
    let expr = state.pop().unwrap();
    state.push(FrameTag::Done.into());
    state.push(expr);
    
    loop {
        match op {
            Op::Eval => match state.peek().unwrap().unpack() {
                UnpackedValue::ORef(oref) => match oref.unpack() {
                    UnpackedHeapValue::Pair(pair) => if let Ok(sym) = Symbol::try_from(pair.car) {
                        match sym.as_str() {
                            "define" => if let Ok(args) = Pair::try_from(pair.cdr) {
                                if let Ok(name) = Symbol::try_from(args.car) {
                                    if let Ok(rargs) = Pair::try_from(args.cdr) {
                                        let value_expr = rargs.car;

                                        if rargs.cdr == Value::NIL {
                                            state.pop();
                                            state.push(name.into());
                                            state.push(FrameTag::Define.into());
                                            state.push(value_expr);
                                        } else {
                                            state.pop();
                                            state.raise(())?;
                                        }
                                    } else {
                                        state.pop();
                                        state.raise(())?;
                                    }
                                } else {
                                    state.pop();
                                    state.raise(())?;
                                }
                            } else {
                                state.pop();
                                state.raise(())?;
                            },
                            "set!" => if let Ok(args) = Pair::try_from(pair.cdr) {
                                if let Ok(name) = Symbol::try_from(args.car) {
                                    if let Ok(rargs) = Pair::try_from(args.cdr) {
                                        let value_expr = rargs.car;

                                        if rargs.cdr == Value::NIL {
                                            state.pop();
                                            state.push(name.into());
                                            state.push(FrameTag::Set.into());
                                            state.push(value_expr);
                                        } else {
                                            state.pop();
                                            state.raise(())?;
                                        }
                                    } else {
                                        state.pop();
                                        state.raise(())?;
                                    }
                                } else {
                                    state.pop();
                                    state.raise(())?;
                                }
                            } else {
                                state.pop();
                                state.raise(())?;
                            },
                            "if" => if let Ok(args) = Pair::try_from(pair.cdr) {
                                let condition = args.car;

                                if let Ok(branches) = Pair::try_from(args.cdr) {
                                    let succeed = branches.car;

                                    if let Ok(rargs) = Pair::try_from(branches.cdr) {
                                        let fail = rargs.car;

                                        if rargs.cdr == Value::NIL {
                                            state.pop();
                                            state.push(succeed);
                                            state.push(fail);
                                            state.push(FrameTag::CondBranch.into());
                                            state.push(condition);
                                        } else {
                                            state.pop();
                                            state.raise(())?
                                        }
                                    } else {
                                        state.pop();
                                        state.raise(())?
                                    }
                                } else {
                                    state.pop();
                                    state.raise(())?
                                }
                            } else {
                                state.pop();
                                state.raise(())?
                            },
                            "quote" => if let Ok(args) = Pair::try_from(pair.cdr) {
                                if args.cdr == Value::NIL {
                                    state.pop();
                                    state.push(args.car);
                                    {op = Op::Continue; state.push(1usize.try_into().unwrap());};
                                } else {
                                    state.pop();
                                    state.raise(())?
                                }
                            } else {
                                state.raise(())?
                            },
                            _ => unimplemented!()
                        }
                    } else {
                        unimplemented!()
                    },
                    UnpackedHeapValue::Symbol(_) => {
                        state.lookup()?;
                        state.push(1usize.try_into().unwrap());
                        op = Op::Continue;
                    },
                    UnpackedHeapValue::Vector(_) => {op = Op::Continue; state.push(1usize.try_into().unwrap());},
                    UnpackedHeapValue::String(_) => {op = Op::Continue; state.push(1usize.try_into().unwrap());},
                    _ => unimplemented!()
                },
                UnpackedValue::Fixnum(_) => {op = Op::Continue; state.push(1usize.try_into().unwrap());},
                UnpackedValue::Flonum(_) => {op = Op::Continue; state.push(1usize.try_into().unwrap());},
                UnpackedValue::Char(_) => {op = Op::Continue; state.push(1usize.try_into().unwrap());},
                UnpackedValue::Bool(_) => {op = Op::Continue; state.push(1usize.try_into().unwrap());},
                UnpackedValue::Unbound => unreachable!(),
                UnpackedValue::Unspecified => {op = Op::Continue; state.push(1usize.try_into().unwrap());},
                UnpackedValue::Eof => {op = Op::Continue; state.push(1usize.try_into().unwrap());},
                UnpackedValue::Nil => {state.pop(); state.raise(())?},
            }

            Op::Continue => {
                let value_count = state.pop().unwrap().try_into().unwrap();
                match unsafe { transmute::<Value, FrameTag>(*state.get(value_count).unwrap()) } {
                    FrameTag::Done => if value_count == 1 {
                        let res = state.pop().unwrap();
                        state.pop();
                        state.push(res);
                        break;
                    } else {
                        for _ in 0..value_count { state.pop(); }
                        state.raise(())?;
                    },
                    FrameTag::Define => if value_count == 1 {
                        let value = state.pop().unwrap();
                        state.pop();
                        state.push(value);
                        unsafe { state.define(); }
                        state.push(Value::try_from(1isize).unwrap());
                    } else {
                        for _ in 0..value_count { state.pop(); }
                        state.raise(())?;
                    },
                    FrameTag::Set => if value_count == 1 {
                        let value = state.pop().unwrap();
                        state.pop();
                        state.push(value);
                        state.set()?;
                        state.push(Value::try_from(1isize).unwrap());
                    } else {
                        for _ in 0..value_count { state.pop(); }
                        state.raise(())?;
                    },
                    FrameTag::CondBranch => if value_count == 1 {
                        if state.pop().unwrap() != Value::FALSE {
                            state.pop(); // frame tag
                            state.pop(); // #f branch
                        } else {
                            state.pop(); // frame tag
                            let branch = state.pop().unwrap();
                            state.pop(); // non-#f branch
                            state.push(branch);
                        }
                        op = Op::Eval;
                    } else {
                        for _ in 0..value_count { state.pop(); }
                        state.raise(())?;
                    }
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_const() {
        let mut state = State::new(1 << 12, 1 << 20, true);
        
        state.push(Value::from('a'));
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from('a'));

        unsafe { state.push_string("foo") };
        eval(&mut state).unwrap();
        let res: PgsString = state.pop().unwrap().try_into().unwrap();

        assert_eq!(res.as_str(), "foo");
    }

    #[test]
    fn test_nil() {
        let mut state = State::new(1 << 12, 1 << 20, true);
        
        state.push(Value::NIL);
        let res = eval(&mut state);

        assert!(res.is_err());
    }

    #[test]
    fn test_variables() {
        let mut state = State::new(1 << 12, 1 << 20, true);

        let mut parser = Parser::new(Lexer::new("(define foo 5)").peekable());
        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();
        assert_eq!(state.pop().unwrap(), Value::UNSPECIFIED);

        let mut parser = Parser::new(Lexer::new("(set! foo 8)").peekable());
        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();
        assert_eq!(state.pop().unwrap(), Value::UNSPECIFIED);

        let mut parser = Parser::new(Lexer::new("foo").peekable());
        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();
        assert_eq!(state.pop().unwrap(), Value::try_from(8isize).unwrap());
    }

    #[test]
    fn test_quote() {
        let mut state = State::new(1 << 12, 1 << 20, true);

        let mut parser = Parser::new(Lexer::new("'()").peekable());

        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::NIL);

        let mut parser = Parser::new(Lexer::new("(quote () ())").peekable());

        parser.sexpr(&mut state).unwrap();
        assert!(eval(&mut state).is_err());
    }

    #[test]
    fn test_if() {
        let mut state = State::new(1 << 12, 1 << 20, true);

        let mut parser = Parser::new(Lexer::new("(if #t 42 23)").peekable());

        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::try_from(42isize).unwrap());

        let mut parser = Parser::new(Lexer::new("(if #f 42)").peekable());

        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::UNSPECIFIED);
    }
}


use std::convert::{TryFrom, TryInto};
use std::mem::transmute;

use super::state::State;
use super::objects::{UnpackedHeapValue, PgsString, Symbol, Pair, Closure, Code, Vector};
use super::refs::{Value, UnpackedValue, FrameTag};

pub enum Op {Eval, Continue, Apply}

pub fn eval(state: &mut State) -> Result<(), ()> {
    let mut op = Op::Eval;
    let expr = state.pop().unwrap();
    state.push_env();
    state.push(FrameTag::Done);
    state.push(expr);
    
    loop {
        match op {
            Op::Eval => match state.peek().unwrap().unpack() {
                UnpackedValue::ORef(oref) => match oref.unpack() {
                    UnpackedHeapValue::Pair(pair) => {
                        if let Ok(sym) = Symbol::try_from(pair.car) {
                            match sym.as_str() {
                                "define" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        // FIXME: Fail if not on toplevel
                                        if let Ok(name) = Symbol::try_from(args.car) {
                                            if let Ok(rargs) = Pair::try_from(args.cdr) {
                                                let value_expr = rargs.car;

                                                if rargs.cdr == Value::NIL {
                                                    state.pop();
                                                    state.push(name);
                                                    state.push_env();
                                                    state.push(FrameTag::Define);
                                                    state.push(value_expr);
                                                    continue;
                                                } 
                                            }
                                        }
                                    }

                                    state.raise(())?;
                                },
                                "set!" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        if let Ok(name) = Symbol::try_from(args.car) {
                                            if let Ok(rargs) = Pair::try_from(args.cdr) {
                                                let value_expr = rargs.car;

                                                if rargs.cdr == Value::NIL {
                                                    state.pop();
                                                    state.push(name);
                                                    state.push_env();
                                                    state.push(FrameTag::Set);
                                                    state.push(value_expr);
                                                    continue;
                                                }
                                            }
                                        }
                                    }

                                    state.raise(())?;
                                },
                                "begin" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        let stmt = args.car;

                                        if args.cdr == Value::NIL {
                                            state.pop();
                                            state.push(stmt);
                                            continue;
                                        } else {
                                            state.pop();
                                            state.push(args.cdr);
                                            state.push_env();
                                            state.push(FrameTag::Stmt);
                                            state.push(stmt);
                                            continue;
                                        }
                                    }

                                    state.raise(())?;
                                },
                                "if" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        let condition = args.car;

                                        if let Ok(branches) = Pair::try_from(args.cdr) {
                                            let succeed = branches.car;

                                            if let Ok(rargs) = Pair::try_from(branches.cdr) {
                                                let fail = rargs.car;

                                                if rargs.cdr == Value::NIL {
                                                    state.pop();
                                                    state.push(succeed);
                                                    state.push(fail);
                                                    state.push_env();
                                                    state.push(FrameTag::CondBranch);
                                                    state.push(condition);
                                                    continue;
                                                }
                                            }
                                        }
                                    }

                                    state.raise(())?
                                },
                                "quote" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        if args.cdr == Value::NIL {
                                            state.pop();
                                            state.push(args.car);
                                            {op = Op::Continue; state.push(1u16);};
                                            continue;
                                        }
                                    }

                                    state.raise(())?
                                },
                                "let" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        let bindings = args.car;

                                        if let Ok(rargs) = Pair::try_from(args.cdr) {
                                            let body = rargs.car;

                                            if rargs.cdr == Value::NIL {
                                                if let Ok(bindings) = Pair::try_from(bindings) {
                                                    let binding = bindings.car;

                                                    if let Ok(binding) = Pair::try_from(binding) {
                                                        let binder = binding.car;

                                                        if let Ok(exprs) = Pair::try_from(binding.cdr) {
                                                            let expr = exprs.car;

                                                            if exprs.cdr == Value::NIL {
                                                                state.pop();
                                                                state.push(body);
                                                                state.push(bindings.cdr);
                                                                state.push(binder);
                                                                state.push(0u16);
                                                                state.push_env();
                                                                state.push(FrameTag::Let);
                                                                state.push(expr);
                                                                continue;
                                                            }
                                                        }
                                                    }
                                                } else if bindings == Value::NIL {
                                                    state.pop();
                                                    state.push(body);
                                                    continue;
                                                }
                                            }
                                        }
                                    }

                                    state.raise(())?
                                },
                                "lambda" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        let params = args.car;

                                        if let Ok(rargs) = Pair::try_from(args.cdr) {
                                            let body = rargs.car;

                                            if rargs.cdr == Value::NIL {
                                                state.pop();
                                                state.push_env();
                                                state.push(body);

                                                let mut params = params;
                                                let mut arity = 0;

                                                while let Ok(param_pair) = Pair::try_from(params) {
                                                    if let Ok(param) = Symbol::try_from(param_pair.car) {
                                                        arity += 1;
                                                        params = param_pair.cdr;
                                                        state.push(param);
                                                    } else {
                                                        state.raise(())?;
                                                    }
                                                }

                                                unsafe { state.vector(arity); }

                                                if params == Value::NIL {
                                                    state.push(Value::FALSE);
                                                } else {
                                                    if let Ok(rest_param) = Symbol::try_from(params) {
                                                        state.push(rest_param);
                                                    } else {
                                                        state.raise(())?;
                                                    }
                                                }

                                                unsafe { state.closure(Code::ApplySelf as usize, 4); }
                                                state.push(1u16);
                                                op = Op::Continue;
                                                continue;
                                            }
                                        }
                                    }

                                    state.raise(())?
                                },
                                _ => {}
                            }
                        }

                        state.pop();
                        state.push(pair.cdr);
                        state.push(0u16);
                        state.push_env();
                        state.push(FrameTag::Arg);
                        state.push(pair.car);
                    },
                    UnpackedHeapValue::Symbol(_) => {
                        state.lookup()?;
                        state.push(1u16);
                        op = Op::Continue;
                    },
                    UnpackedHeapValue::Vector(_) => {op = Op::Continue; state.push(1u16);},
                    UnpackedHeapValue::String(_) => {op = Op::Continue; state.push(1u16);},
                    _ => unimplemented!()
                },
                UnpackedValue::Fixnum(_) => {op = Op::Continue; state.push(1u16);},
                UnpackedValue::Flonum(_) => {op = Op::Continue; state.push(1u16);},
                UnpackedValue::Char(_) => {op = Op::Continue; state.push(1u16);},
                UnpackedValue::Bool(_) => {op = Op::Continue; state.push(1u16);},
                UnpackedValue::Unspecified => {op = Op::Continue; state.push(1u16);},
                UnpackedValue::Eof => {op = Op::Continue; state.push(1u16);},
                UnpackedValue::Nil => state.raise(())?,
                UnpackedValue::Unbound => unreachable!(),
                UnpackedValue::FrameTag(_) => unreachable!()
            }

            Op::Continue => {
                let value_count = state.pop().unwrap().try_into().unwrap();
                state.set_env(state.get(value_count + 1).unwrap().try_into().unwrap());
                match unsafe { transmute::<Value, FrameTag>(state.get(value_count).unwrap()) } {
                    FrameTag::Done => if value_count == 1 {
                        let res = state.pop().unwrap();
                        state.pop(); // frame tag
                        state.pop(); // env
                        state.push(res);
                        break;
                    } else {
                        state.raise(())?;
                    },
                    FrameTag::Define => if value_count == 1 {
                        let value = state.pop().unwrap();
                        state.pop(); // frame tag
                        state.pop(); // env
                        state.push(value);
                        unsafe { state.define(); }
                        state.push(Value::UNSPECIFIED);
                        state.push(1u16);
                    } else {
                        state.raise(())?;
                    },
                    FrameTag::Set => if value_count == 1 {
                        let value = state.pop().unwrap();
                        state.pop(); // frame tag
                        state.pop(); // env
                        state.push(value);
                        state.set()?;
                        state.push(1u16);
                    } else {
                        state.raise(())?;
                    },
                    FrameTag::CondBranch => if value_count == 1 {
                        if state.pop().unwrap() != Value::FALSE {
                            state.pop(); // frame tag
                            state.pop(); // env
                            state.pop(); // #f branch
                        } else {
                            state.pop(); // frame tag
                            state.pop(); // env
                            let branch = state.pop().unwrap();
                            state.pop(); // non-#f branch
                            state.push(branch);
                        }
                        op = Op::Eval;
                    } else {
                        state.raise(())?;
                    },
                    FrameTag::Let => if value_count == 1 {
                        let value = state.pop().unwrap();
                        let i: usize = state.get(2).unwrap().try_into().unwrap();

                        if let Ok(name) = Symbol::try_from(state.get(3 + i).unwrap()) {
                            let bindings = state.get(4 + i).unwrap();

                            if let Ok(bindings) = Pair::try_from(bindings) {
                                let binding = bindings.car;

                                if let Ok(binding) = Pair::try_from(binding) {
                                    let binder = binding.car;

                                    if let Ok(exprs) = Pair::try_from(binding.cdr) {
                                        let expr = exprs.car;

                                        if exprs.cdr == Value::NIL {
                                            state.put(3 + i, binder)?;
                                            state.put(4 + i, bindings.cdr)?;
                                            state.pop(); // frame tag
                                            state.pop(); // env
                                            state.pop(); // i 
                                            state.push(name);
                                            state.push(value);
                                            state.push(Value::try_from(i + 2).unwrap());
                                            state.push_env();
                                            state.push(FrameTag::Let);
                                            state.push(expr);
                                            op = Op::Eval;
                                        } else {
                                            state.raise(())?;
                                        }
                                    } else {
                                        state.raise(())?;
                                    }
                                }
                            } else if bindings == Value::NIL {
                                state.pop(); // frame tag
                                state.pop(); // env
                                state.pop(); // i
                                state.push(name);
                                state.push(value);
                                unsafe { state.push_scope(); }
                                // FIXME: It is an error for a <variable> to appear more than once
                                for _ in 0..i / 2 + 1 { unsafe { state.define(); } }
                                state.pop(); // name
                                state.pop(); // bindings
                                op = Op::Eval;
                            } else {
                                state.raise(())?;
                            }
                        } else {
                            state.raise(())?;
                        }
                    } else {
                        state.raise(())?;
                    },
                    FrameTag::Stmt => {
                        let stmts = state.get(value_count + 2).unwrap();

                        if let Ok(stmts) = Pair::try_from(stmts) {
                            for _ in 0..value_count { state.pop(); }
                            state.put(2, stmts.cdr)?;
                            state.push(stmts.car);
                            op = Op::Eval;
                        } else if stmts == Value::NIL {
                            for _ in 0..(FrameTag::Stmt.framesize().0 + 1) {
                                state.remove(value_count);
                            }
                            state.push(Value::try_from(value_count).unwrap());
                        } else {
                            state.raise(())?;
                        }
                    },
                    FrameTag::Arg => if value_count == 1 {
                        let value = state.pop().unwrap();
                        let i: usize = state.get(2).unwrap().try_into().unwrap();
                        let rargs = state.get(3 + i).unwrap();

                        // OPTIMIZE:
                        if let Ok(rargs) = Pair::try_from(rargs) {
                            state.pop(); // frame tag
                            state.pop(); // env
                            state.pop(); // i
                            state.put(i, rargs.cdr)?;
                            state.push(value);
                            state.push(Value::try_from(i + 1).unwrap());
                            state.push_env();
                            state.push(FrameTag::Arg);
                            state.push(rargs.car);
                            op = Op::Eval;
                        } else if rargs == Value::NIL {
                            state.pop(); // frame tag
                            state.pop(); // env
                            state.pop(); // i
                            state.push(value);
                            state.push(Value::try_from(i).unwrap()); // argc for apply
                            state.remove(i + 2); // rargs
                            op = Op::Apply;
                        } else {
                            state.raise(())?;
                        }
                    } else {
                        state.raise(())?;
                    }
                }
            },
            Op::Apply => { // ... callee arg{argc} argc
                let arg_count: usize = state.peek().unwrap().try_into().unwrap();

                if let Ok(callee) = Closure::try_from(state.get(arg_count + 1).unwrap()) {
                    match Code::try_from(callee.code) {
                        Ok(Code::ApplySelf) => match callee.clovers() {
                            &[env, body, params, rest_param] => {
                                let params: Vector = params.try_into().unwrap();

                                if arg_count == params.len()
                                   || rest_param != Value::FALSE && arg_count >= params.len()
                                {
                                    // FIXME: It is an error for a <variable> to appear more than once
                                    state.pop(); // argc
                                    state.set_env(env.try_into().unwrap());
                                    unsafe { state.push_scope(); }

                                    if rest_param != Value::FALSE {
                                        state.push(Value::NIL);
                                        for _ in 0..arg_count - params.len() {
                                            unsafe { state.cons(); }
                                        }
                                        state.push(rest_param);
                                        state.swap();
                                        unsafe { state.define(); }
                                    }

                                    for &param in params.iter().rev() {
                                        state.push(param);
                                        state.swap();
                                        unsafe { state.define(); }
                                    }

                                    state.pop(); // callee
                                    state.push(body);
                                    op = Op::Eval;
                                } else {
                                    state.raise(())?;
                                }
                            },
                            _ => unreachable!()
                        },
                        Err(_) => unimplemented!()
                    }
                } else {
                    state.raise(())?;
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
        let mut state = State::new(1 << 12, 1 << 20);
        
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
        let mut state = State::new(1 << 12, 1 << 20);
        
        state.push(Value::NIL);
        let res = eval(&mut state);

        assert!(res.is_err());
    }

    #[test]
    fn test_variables() {
        let mut state = State::new(1 << 12, 1 << 20);

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
        assert_eq!(state.pop().unwrap(), Value::from(8i16));
    }

    #[test]
    fn test_quote() {
        let mut state = State::new(1 << 12, 1 << 20);

        let mut parser = Parser::new(Lexer::new("'()").peekable());

        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::NIL);

        let mut parser = Parser::new(Lexer::new("(quote () ())").peekable());

        parser.sexpr(&mut state).unwrap();
        assert!(eval(&mut state).is_err());
    }

    #[test]
    fn test_begin() {
        let mut state = State::new(1 << 12, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(begin 42 23)").peekable());

        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from(23i16));

        let mut parser = Parser::new(Lexer::new("(begin)").peekable());

        parser.sexpr(&mut state).unwrap();
        assert!(eval(&mut state).is_err());
    }

    #[test]
    fn test_if() {
        let mut state = State::new(1 << 12, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(if #t 42 23)").peekable());

        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from(42i16));
    }

    #[test]
    fn test_let() {
        let mut state = State::new(1 << 12, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(let ((a (if #f 5 8)) (b #f)) (if b 42 a))").peekable());

        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from(8i16));
    }

    #[test]
    fn test_lambda() {
        let mut state = State::new(1 << 12, 1 << 20);

        let mut parser = Parser::new(Lexer::new("((lambda (a b) b) 5 8)").peekable());
 
        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from(8i16));
    }
}


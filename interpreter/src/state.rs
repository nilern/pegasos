use std::convert::{TryFrom, TryInto};
use std::env;
use std::io;
use std::iter;
use std::mem::{size_of, transmute};
use std::path::{Path, PathBuf};

use super::error::PgsError;
use super::gc::MemoryManager;
use super::interpreter::{Primitive, RuntimeError};
use super::objects::{
    Bindings, Cars, Closure, Object, Pair, PgsString, Record, Symbol, SymbolTable, Syntax, Vector
};
use super::parser::Loc;
use super::primitives::PRIMITIVES;
use super::refs::{FrameTag, HeapValue, Value};

pub struct State {
    heap: MemoryManager<Object>,
    symbol_table: SymbolTable,
    stack: Vec<Value>,
    env: Bindings
}

impl State {
    const STACK_SIZE: usize = 1 << 12; // 4 kiB
    const STACK_LEN: usize = Self::STACK_SIZE / size_of::<Value>();

    pub fn new(path: &[PathBuf], initial_heap: usize, max_heap: usize) -> Self {
        let mut res = Self {
            heap: MemoryManager::new(initial_heap, max_heap),
            symbol_table: SymbolTable::new(),
            stack: Vec::with_capacity(Self::STACK_LEN),
            env: unsafe { transmute(Value::UNBOUND) } // HACK
        };
        res.env = Bindings::new(&mut res, None).unwrap();

        unsafe {
            res.push_symbol("*include-path*");
            for dir in path {
                res.push_string(dir.to_str().unwrap());
            }
            res.push(Value::NIL);
            for _ in path {
                res.cons();
            }
            res.define();

            for (name, code) in PRIMITIVES.iter() {
                res.push_symbol(name);
                res.push_primitive(*code);
                res.define();
            }
        }

        res
    }

    pub fn push<T: Into<Value>>(&mut self, v: T) {
        if self.stack.len() < self.stack.capacity() {
            self.stack.push(v.into());
        } else {
            unimplemented!() // FIXME: Handle stack overflow
        }
    }

    pub fn pop(&mut self) -> Option<Value> {
        // FIXME: handle stack underflow
        self.stack.pop()
    }

    pub fn peek(&self) -> Option<Value> { self.stack.last().map(|&v| v) }

    pub fn swap(&mut self) {
        let len = self.stack.len();
        self.stack.swap(len - 2, len - 1);
    }

    pub fn dup(&mut self) { self.push(self.peek().unwrap()) }

    pub fn get(&self, i: usize) -> Option<Value> {
        self.stack.len().checked_sub(1 + i).map(|i| self.stack[i])
    }

    pub fn put(&mut self, i: usize, v: Value) {
        let i = self.stack.len().checked_sub(1 + i).unwrap();
        self.stack[i] = v;
    }

    pub fn insert_after(&mut self, i: usize, v: Value) {
        let i = self.stack.len().checked_sub(i).unwrap();
        self.stack.insert(i, v)
    }

    pub fn remove(&mut self, i: usize) -> Option<Value> {
        self.stack.len().checked_sub(1 + i).map(|i| self.stack.remove(i))
    }

    pub fn alloc<T: ?Sized>(&mut self, base: Object) -> Option<HeapValue<T>> {
        self.heap.alloc(base).map(|v| unsafe { transmute(v) })
    }

    pub unsafe fn push_string(&mut self, s: &str) {
        let v = PgsString::new(self, s).unwrap_or_else(|| {
            self.collect_garbage();
            PgsString::new(self, s).unwrap()
        });
        self.push(v)
    }

    pub unsafe fn push_symbol(&mut self, name: &str) {
        let v = Symbol::new(self, name).unwrap_or_else(|| {
            self.collect_garbage();
            Symbol::new(self, name).unwrap()
        });
        self.push(v)
    }

    pub unsafe fn cons(&mut self) {
        debug_assert!(self.stack.len() >= 2);

        let mut pair = Pair::new(self).unwrap_or_else(|| {
            self.collect_garbage();
            Pair::new(self).unwrap()
        });
        pair.cdr = self.stack.pop().unwrap();
        pair.car = self.stack.pop().unwrap();
        self.push(pair);
    }

    pub unsafe fn push_vector(&mut self, len: usize) {
        let vec = Vector::new(self, len).unwrap_or_else(|| {
            self.collect_garbage();
            Vector::new(self, len).unwrap()
        });
        self.push(vec);
    }

    pub unsafe fn vector(&mut self, len: usize) {
        debug_assert!(self.stack.len() >= len);

        let mut vec = Vector::new(self, len).unwrap_or_else(|| {
            self.collect_garbage();
            Vector::new(self, len).unwrap()
        });
        vec.copy_from_slice(&self.stack[self.stack.len() - len..]);
        self.stack.truncate(self.stack.len() - len);
        self.push(vec)
    }

    pub unsafe fn closure(&mut self, code: usize, len: usize) {
        debug_assert!(self.stack.len() >= len);

        let mut f = Closure::new(self, code, len).unwrap_or_else(|| {
            self.collect_garbage();
            Closure::new(self, code, len).unwrap()
        });
        f.clovers_mut().copy_from_slice(&self.stack[self.stack.len() - len..]);
        self.stack.truncate(self.stack.len() - len);
        self.stack.push(f.into())
    }

    pub unsafe fn push_primitive(&mut self, code: Primitive) {
        let code = code as usize;
        let f = Closure::new(self, code, 0).unwrap_or_else(|| {
            self.collect_garbage();
            Closure::new(self, code, 0).unwrap()
        });
        self.push(f);
    }

    pub unsafe fn push_syntax(&mut self, loc: Loc) {
        let datum = self.peek().unwrap();
        self.push(loc.source);
        let syntax = if let Some(syntax) = Syntax::new(
            self,
            datum,
            Value::FALSE,
            loc.source,
            loc.pos.line.try_into().unwrap(),
            loc.pos.column.try_into().unwrap()
        ) {
            self.pop().unwrap(); // source
            self.pop().unwrap(); // datum
            syntax
        } else {
            self.collect_garbage();
            let source = self.pop().unwrap();
            let datum = self.pop().unwrap();
            Syntax::new(
                self,
                datum,
                Value::FALSE,
                source,
                loc.pos.line.try_into().unwrap(),
                loc.pos.column.try_into().unwrap()
            )
            .unwrap()
        };
        self.push(syntax);
    }

    pub unsafe fn record(&mut self, len: usize) {
        let mut res = Record::new(self, len).unwrap_or_else(|| {
            self.collect_garbage();
            Record::new(self, len).unwrap()
        });
        res.slots_mut().copy_from_slice(&self.stack[self.stack.len() - len..]);
        self.stack.truncate(self.stack.len() - len);
        self.push(res);
    }

    pub fn get_symbol(&mut self, name: &str) -> Option<Symbol> {
        self.symbol_table.get(&mut self.heap, name)
    }

    pub fn push_env(&mut self) { self.push(self.env) }

    pub fn env(&self) -> Bindings { self.env }

    pub fn set_env(&mut self, env: Bindings) { self.env = env }

    pub unsafe fn push_scope(&mut self) {
        let env = Bindings::new(self, Some(self.env)).unwrap_or_else(|| {
            self.collect_garbage();
            Bindings::new(self, Some(self.env)).unwrap()
        });
        self.env = env;
    }

    pub fn lookup(&mut self) -> Result<(), RuntimeError> {
        let name = unsafe { transmute::<Value, Symbol>(self.pop().unwrap()) }; // checked before call
        self.env.get(name).map(|v| self.push(v)).ok_or(RuntimeError::Unbound(name))
    }

    pub unsafe fn define(&mut self) {
        let value = self.pop().unwrap();
        let name = transmute::<Value, Symbol>(self.pop().unwrap()); // its type was checked before pushing it
        self.env.insert(self, name, value).unwrap_or_else(|_| {
            self.push(name);
            self.push(value);
            self.collect_garbage();
            let value = self.pop().unwrap();
            let name = transmute::<Value, Symbol>(self.pop().unwrap());
            self.env.insert(self, name, value).unwrap()
        });
    }

    pub fn set(&mut self) -> Result<(), RuntimeError> {
        let value = self.pop().unwrap();
        let name = unsafe { transmute::<Value, Symbol>(self.pop().unwrap()) }; // its type was checked before pushing it
        self.env.set(name, value).map_err(|()| RuntimeError::Unbound(name))?;
        Ok(self.stack.push(Value::UNSPECIFIED))
    }

    pub fn raise<'a, T, E: Into<PgsError>>(&mut self, err: E) -> Result<T, PgsError> {
        // TODO: Actually try raising Scheme exception.
        Err(err.into())
    }

    pub fn unwind(&mut self) {
        // TODO: Also unwind env to top level
        self.stack.truncate(0);
    }

    // TODO: References from symbol table should be weak
    pub unsafe fn collect_garbage(&mut self) {
        self.heap
            .collection()
            .roots(self.stack.iter_mut().map(|v| v as *mut Value))
            .roots(iter::once(transmute::<&mut Bindings, *mut Value>(&mut self.env)))
            .traverse()
            .weaks(self.symbol_table.iter_mut().map(|v| transmute::<&mut Symbol, *mut Value>(v)));
    }

    pub unsafe fn dump<W: io::Write>(&self, dest: &mut W) -> io::Result<()> {
        writeln!(dest, "environment:\n")?;
        self.env.dump(dest)?;

        writeln!(dest, "")?;

        writeln!(dest, "stack trace:\n")?;
        self.print_stacktrace(dest)?;

        Ok(())
    }

    pub unsafe fn print_stacktrace<W: io::Write>(&self, dest: &mut W) -> io::Result<()> {
        for (tag, slots) in self.stack_frames() {
            writeln!(dest, "{:?}", tag)?;

            for slot in slots.iter().rev() {
                writeln!(dest, "\t{}", slot)?;
            }
        }

        Ok(())
    }

    unsafe fn stack_frames(&self) -> Frames {
        Frames { stack: &self.stack, index: self.stack.len() - 1, done: self.stack.len() < 1 }
    }

    pub fn resolve_path(&mut self, filename: &str) -> Option<PathBuf> {
        fn step(dir: &Path, filename: &str) -> Option<PathBuf> {
            let mut path = PathBuf::new();
            path.push(dir);
            path.push(filename);
            if path.exists() {
                Some(path)
            } else {
                None
            }
        }

        let ores = step(&env::current_dir().unwrap(), filename);
        if ores.is_some() {
            return ores;
        }

        let include_path = self.get_symbol("*include-path*")?;
        for dir in Cars::of(self.env.get(include_path)?) {
            let dir = PgsString::try_from(dir).expect("non-string in *include-path*"); // HACK

            let ores = step(dir.as_ref(), filename);
            if ores.is_some() {
                return ores;
            }
        }

        None
    }
}

struct Frames<'a> {
    stack: &'a [Value],
    index: usize,
    done: bool
}

impl<'a> Iterator for Frames<'a> {
    type Item = (FrameTag, &'a [Value]);

    fn next(&mut self) -> Option<Self::Item> {
        if !self.done {
            loop {
                if self.stack[self.index].is_frame_tag() {
                    break;
                } else if self.index == 0 {
                    self.done = true;
                    return None;
                } else {
                    self.index -= 1;
                }
            }

            let i = self.index;
            let tag = unsafe { transmute::<Value, FrameTag>(self.stack[i]) };
            let (base_len, dynamic) = tag.framesize();
            let len = if dynamic {
                let dyn_len: usize = self.stack[i - 2].try_into().unwrap();
                base_len + dyn_len + 1
            } else {
                base_len
            };
            if len + 1 >= i {
                self.done = true;
            } else {
                self.index -= len + 1;
            }
            Some((tag, &self.stack[i - len..i]))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryFrom;

    #[test]
    fn test_gc() {
        let mut state = State::new(&[], 1 << 14, 1 << 20);
        let n = 4i16;
        for i in 0..n {
            state.push(i);
        }
        state.push(Value::NIL);
        for _ in 0..n {
            unsafe {
                state.cons();
            }
        }

        unsafe {
            state.collect_garbage();
        }
        unsafe {
            state.collect_garbage();
        } // overwrite initial heap and generally cause more havoc

        let mut ls = state.pop().unwrap();
        for i in 0..n {
            if let Ok(pair) = Pair::try_from(ls) {
                assert_eq!(pair.car, i.into());
                ls = pair.cdr;
            } else {
                assert!(false);
            }
        }
        assert_eq!(ls, Value::NIL);
    }
}

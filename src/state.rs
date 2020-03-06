use std::iter;
use std::mem::{size_of, transmute};

use super::gc::MemoryManager;
use super::value::{Value, HeapValue, Object, PgsString, Symbol, Pair, Vector, SymbolTable};
use super::bindings::Bindings;

pub struct State {
    heap: MemoryManager<Object>,
    symbol_table: SymbolTable,
    stack: Vec<Value>,
    env: Bindings
}

impl State {
    const STACK_SIZE: usize = 1 << 12; // 4 kiB
    const STACK_LEN: usize = Self::STACK_SIZE / size_of::<Value>();

    pub fn new(initial_heap: usize, max_heap: usize) -> Self {
        let mut res = Self {
            heap: MemoryManager::new(initial_heap, max_heap),
            symbol_table: SymbolTable::new(),
            stack: Vec::with_capacity(Self::STACK_LEN),
            env: unsafe { transmute(Value::UNBOUND) } // HACK
        };
        res.env = Bindings::new(&mut res).unwrap();
        res
    }

    pub fn push(&mut self, v: Value) {
        if self.stack.len() < self.stack.capacity() {
            self.stack.push(v);
        } else {
            unimplemented!() // FIXME: Handle stack overflow
        }
    }

    pub fn pop(&mut self) -> Option<Value> {
        // FIXME: handle stack underflow
        self.stack.pop()
    }

    pub fn peek(&self) -> Option<&Value> { self.stack.last() }

    pub fn get(&self, i: usize) -> Option<&Value> {
        self.stack.len().checked_sub(1 + i).and_then(|i| self.stack.get(i))
    }

    pub fn alloc<T>(&mut self, base: Object) -> Option<HeapValue<T>> {
        self.heap.alloc(base).map(|v| unsafe { transmute(v) })
    }

    pub unsafe fn push_string(&mut self, s: &str) {
        let v = PgsString::new(self, s).unwrap_or_else(|| {
            self.collect_garbage();
            PgsString::new(self, s).unwrap()
        });
        self.push(v.into())
    }

    pub unsafe fn push_symbol(&mut self, name: &str) {
        let v = Symbol::new(&mut self.heap, &mut self.symbol_table, name).unwrap_or_else(|| {
            self.collect_garbage();
            Symbol::new(&mut self.heap, &mut self.symbol_table, name).unwrap()
        });
        self.push(v.into())
    }

    pub unsafe fn cons(&mut self) {
        debug_assert!(self.stack.len() >= 2);

        let mut pair = Pair::new(self).unwrap_or_else(|| {
            self.collect_garbage();
            Pair::new(self).unwrap()
        });
        pair.cdr = self.stack.pop().unwrap();
        pair.car = self.stack.pop().unwrap();
        self.stack.push(pair.into());
    }

    pub unsafe fn vector(&mut self, len: usize) {
        debug_assert!(self.stack.len() >= len);
        
        let mut vec = Vector::new(self, len).unwrap_or_else(|| {
            self.collect_garbage();
            Vector::new(self, len).unwrap()
        });
        vec.copy_from_slice(&self.stack[self.stack.len() - len..]);
        self.stack.truncate(self.stack.len() - len);
        self.stack.push(vec.into())
    }

    pub fn lookup(&mut self) -> Result<(), ()> {
        let name = unsafe { transmute::<Value, Symbol>(self.pop().unwrap()) }; // checked before call
        self.env.get(name).map(|v| self.push(v)).ok_or(())
    }

    pub unsafe fn define(&mut self) {
        let value = self.pop().unwrap();
        let name = transmute::<Value, Symbol>(self.pop().unwrap()); // its type was checked before pushing it
        self.env.insert(self, name, value).unwrap_or_else(|_| {
            self.push(name.into());
            self.push(value);
            self.collect_garbage();
            let value = self.pop().unwrap();
            let name = transmute::<Value, Symbol>(self.pop().unwrap());
            self.env.insert(self, name, value).unwrap()
        });
        self.push(Value::UNSPECIFIED)
    }

    pub fn set(&mut self) -> Result<(), ()> {
        let value = self.pop().unwrap();
        let name = unsafe { transmute::<Value, Symbol>(self.pop().unwrap()) }; // its type was checked before pushing it
        self.env.set(self, name, value)?;
        Ok(self.stack.push(Value::UNSPECIFIED))
    }

    // TODO: References from symbol table should be weak
    pub unsafe fn collect_garbage(&mut self) {
        self.heap.collection()
            .roots(self.stack.iter_mut().map(|v| v as *mut Value))
            .roots(iter::once(transmute::<&mut Bindings, *mut Value>(&mut self.env)))
            .traverse()
            .weaks(self.symbol_table.iter_mut().map(|v| transmute::<&mut Symbol, *mut Value>(v)));
    }
}


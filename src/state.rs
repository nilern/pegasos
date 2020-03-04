use std::mem::{size_of, transmute};

use super::gc::MemoryManager;
use super::value::{Value, HeapValue, Object, PgsString, Symbol, Pair, Vector};

pub struct State {
    heap: MemoryManager<Object>,
    stack: Vec<Value>
}

impl State {
    const STACK_SIZE: usize = 1 << 12; // 4 kiB
    const STACK_LEN: usize = Self::STACK_SIZE / size_of::<Value>();

    pub fn new(initial_heap: usize, max_heap: usize) -> Self {
        Self {
            heap: MemoryManager::new(initial_heap, max_heap),
            stack: Vec::with_capacity(Self::STACK_LEN)
        }
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
        let v = Symbol::new(self, name).unwrap_or_else(|| {
            self.collect_garbage();
            Symbol::new(self, name).unwrap()
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

    pub unsafe fn collect_garbage(&mut self) {
        self.heap.collect_garbage(&mut self.stack);
    }
}


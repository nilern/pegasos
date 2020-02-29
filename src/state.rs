use std::mem::{size_of, transmute};

use super::gc::MemoryManager;
use super::value::{Value, HeapValue, Object};

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

    pub fn alloc<T>(&mut self, base: Object) -> Option<HeapValue<T>> {
        self.heap.alloc(base).map(|v| unsafe { transmute(v) })
    }
}


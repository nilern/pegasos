use std::mem::{size_of, swap, transmute};
use std::ops::{Deref, DerefMut};

use super::state::State;
use super::value::{Value, HeapValue, Vector, Symbol, Header, HeapTag, Object};

#[derive(Clone, Copy)]
pub struct Bindings(pub HeapValue<BindingsData>);

#[repr(C)]
pub struct BindingsData {
    entries: Vector,
    occupancy: Value
}

impl Bindings {
    const VACANT: Value = Value::ZERO;

    pub fn new(state: &mut State) -> Option<Bindings> {
        let base = Object {header: Header::new(HeapTag::Bindings, size_of::<BindingsData>() / size_of::<Value>())};
        state.alloc::<BindingsData>(base)
            .and_then(|mut bindings| {
                Vector::new(state, 2*2).map(|vec| {
                    bindings.entries = vec; // vec is already VACANT (= 0) initialized
                    // occupancy is already 0
                    Bindings(bindings)
                })
            })
    }

    pub fn get(mut self, name: Symbol) -> Option<Value> {
        match self.locate(name) {
            (i, true) => Some(self.entries[2*i + 1]),
            (_, false) => None
        }
    }

    // Returns `Err` when had to grow and allocation failed.
    pub fn insert(mut self, state: &mut State, name: Symbol, value: Value) -> Result<(), ()> {
        self.ensure_vacancy(state)
            .map(|_| self.insert_noresize(name, value))
            .ok_or(())
    }

    fn insert_noresize(mut self, name: Symbol, value: Value) {
        let (i, _) = self.locate(name);
        self.entries[2*i] = name.into();
        self.entries[2*i + 1] = value;
        unsafe {
            self.occupancy = transmute::<usize, Value>(transmute::<Value, usize>(self.occupancy) + (1 << Value::SHIFT));
        }
    }

    fn locate(self, name: Symbol) -> (usize, bool) {
        let hash = name.hash as usize;
        let capacity = self.capacity();

        for collisions in 0..capacity {
            let i = hash + collisions & capacity - 1; // hash + collisions % capacity

            match self.entries[2*i] {
                Self::VACANT => return (i, false),
                k => if k == Value::from(name) {
                    return (i, true);
                }
            }
        }
        unreachable!() // If we got here this was called on a full table
    }

    fn ensure_vacancy(mut self, state: &mut State) -> Option<()> {
        if unsafe { transmute::<Value, usize>(self.occupancy) >> Value::SHIFT } + 1 > self.capacity() / 2 {
            self.rehash(state)
        } else {
            Some(())
        }
    }

    fn rehash(mut self, state: &mut State) -> Option<()> {
        Vector::new(state, 2 * self.entries.len())
            .map(|mut entries| {
                swap(&mut self.entries, &mut entries);

                for entry in entries.chunks_exact(2) {
                    match entry[0] {
                        Self::VACANT => {},
                        name => self.insert_noresize(unsafe { transmute(name) }, entry[1])
                    }
                }
            })
    }

    fn capacity(self) -> usize { self.entries.len() / 2 }
}

impl Deref for Bindings {
    type Target = BindingsData;

    fn deref(&self) -> &Self::Target { &*self.0 }
}

impl DerefMut for Bindings {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut *self.0 }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::{TryFrom, TryInto};

    #[test]
    fn test_bindings() {
        let mut state = State::new(1 << 12, 1 << 20);
        let mut bindings = Bindings::new(&mut state).unwrap();
        let a = Value::try_from(5isize).unwrap();
        let b = Value::try_from(8isize).unwrap();
        let foo = unsafe {state.push_symbol("foo"); state.pop().unwrap().try_into().unwrap()};
        let bar = unsafe {state.push_symbol("bar"); state.pop().unwrap().try_into().unwrap()};

        bindings.insert(&mut state, foo, a).unwrap();
        bindings.insert(&mut state, bar, b).unwrap();

        assert_eq!(bindings.get(foo).unwrap(), a);
        assert_eq!(bindings.get(bar).unwrap(), b);
    }
}


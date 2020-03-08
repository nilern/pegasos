use std::convert::TryFrom;
use std::io;
use std::mem::{size_of, swap, transmute};

use super::state::State;
use super::value::{Heaped, Value, HeapValue, Vector, Symbol, Header, HeapTag, Object};

#[repr(C)]
pub struct BindingsData {
    parent: Value,
    keys: Vector,
    values: Vector,
    occupancy: Value
}

impl Heaped for BindingsData {
    const TAG: HeapTag = HeapTag::Bindings;
}

pub type Bindings = HeapValue<BindingsData>;

impl Bindings {
    pub fn new(state: &mut State, parent: Option<Bindings>) -> Option<Bindings> {
        state.alloc::<BindingsData>(Object::new(Header::new(HeapTag::Bindings, size_of::<BindingsData>() / size_of::<Value>())))
            .and_then(|mut bindings| {
                Vector::new(state, 2).and_then(|keys| {
                    Vector::new(state, 2).map(|values| {
                        bindings.parent = parent.map_or(Value::FALSE, Value::from);
                        bindings.keys = keys; // keys are already VACANT (= 0) initialized
                        bindings.values = values;
                        // occupancy is already 0
                        bindings
                    })
                })
            })
    }

    const VACANT: Value = Value::ZERO;

    pub fn get(self, name: Symbol) -> Option<Value> {
        let mut this = self;
        loop {
            match this.locate(name) {
                (i, true) => return Some(this.values[i]),
                (_, false) => if let Ok(parent) = Bindings::try_from(this.parent) {
                    this = parent;
                } else {
                    return None;
                }
            }
        }
    }

    // Returns `Err` when had to grow and allocation failed.
    pub fn insert(self, state: &mut State, name: Symbol, value: Value) -> Result<(), ()> {
        self.ensure_vacancy(state)
            .map(|_| self.insert_noresize(name, value))
            .ok_or(())
    }

    // Returns `Err` if not found.
    pub fn set(self, name: Symbol, value: Value) -> Result<(), ()> {
        let mut this = self;
        loop {
            match this.locate(name) {
                (i, true) => {
                    this.values[i] = value;
                    return Ok(());
                },
                (_, false) => if let Ok(parent) = Bindings::try_from(this.parent) {
                    this = parent;
                } else {
                    return Err(())
                }
            }
        }
    }

    fn insert_noresize(mut self, name: Symbol, value: Value) {
        let (i, _) = self.locate(name);
        self.keys[i] = name.into();
        self.values[i] = value;
        unsafe {
            self.occupancy = transmute::<usize, Value>(transmute::<Value, usize>(self.occupancy) + (1 << Value::SHIFT));
        }
    }

    fn locate(self, name: Symbol) -> (usize, bool) {
        let hash = name.hash as usize;
        let capacity = self.capacity();

        for collisions in 0..capacity {
            let i = hash + collisions & capacity - 1; // hash + collisions % capacity

            match self.keys[i] {
                Self::VACANT => return (i, false),
                k => if k == Value::from(name) {
                    return (i, true);
                }
            }
        }
        unreachable!() // If we got here this was called on a full table
    }

    fn ensure_vacancy(self, state: &mut State) -> Option<()> {
        if unsafe { transmute::<Value, usize>(self.occupancy) >> Value::SHIFT } + 1 > self.capacity() / 2 {
            self.rehash(state)
        } else {
            Some(())
        }
    }

    fn rehash(mut self, state: &mut State) -> Option<()> {
        let len = 2 * self.keys.len();

        Vector::new(state, len).and_then(|mut keys| {
            Vector::new(state, len).map(|mut values| {
                swap(&mut self.keys, &mut keys);
                swap(&mut self.values, &mut values);

                for (i, &key) in keys.iter().enumerate() {
                    match key {
                        Self::VACANT => {},
                        name => self.insert_noresize(unsafe { transmute(name) }, values[i])
                    }
                }
            })
        })
    }

    fn capacity(self) -> usize { self.keys.len() }

    pub fn dump<W: io::Write>(self, dest: &mut W) -> io::Result<()> {
        for (k, v) in self.keys.iter().zip(self.values.iter()) {
            if *k != Self::VACANT {
                writeln!(dest, "{} = {}", k, v)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::{TryFrom, TryInto};

    #[test]
    fn test_bindings() {
        let mut state = State::new(1 << 12, 1 << 20);
        let bindings = Bindings::new(&mut state, None).unwrap();
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

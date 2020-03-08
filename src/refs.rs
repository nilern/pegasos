use std::char;
use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::{self, size_of, transmute};
use std::ops::{Deref, DerefMut};

use super::gc::{ObjectReference, HeapObject};
use super::objects::{Heaped, Object, HeapTag, Bindings, Closure, Pair, PgsString, Symbol, Vector};
use super::util::fsize;

// ---

#[derive(PartialEq)]
enum BaseTag {
    ORef = 0b01,
    Fixnum = 0b00, // i30 | i62
    Flonum = 0b10, // f30 | f62
    Ext = 0b11     // char, bool, '() etc.
}

#[derive(PartialEq)]
enum Tag {
    ORef = 0b01,
    Fixnum = 0b00,
    Flonum = 0b10,
    Char = 0b0111, // (28 | 60) bit char
    Bool = 0b1011,
    // 0b0011 is unused
    Singleton = 0b1111 // '() etc.
}

pub enum UnpackedValue {
    ORef(HeapValue<()>),
    Fixnum(isize),
    Flonum(fsize),
    Char(char),
    Bool(bool),
    Nil,
    Unbound,
    Unspecified,
    Eof
}

pub enum UnpackedHeapValue {
    Vector(Vector),
    String(PgsString),
    Symbol(Symbol),
    Pair(Pair),
    Bindings(Bindings),
    Closure(Closure)
}

// ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Value(usize);

impl Value {
    pub unsafe fn from_data(ptr: *mut u8) -> Self {
        Self(ptr as usize | BaseTag::ORef as usize)
    }
}

impl ObjectReference for Value {
    type Object = Object;

    unsafe fn from_ptr(ptr: *mut Self::Object) -> Self {
        Self((*ptr).data() as usize | BaseTag::ORef as usize)
    }

    fn as_mut_ptr(self) -> Option<*mut Object> {
        if self.is_oref() {
            Some(unsafe { ((self.0 & !Self::MASK) as *mut Object).offset(-1) })
        } else {
            None
        }
    }
}

impl Value {
    pub const SHIFT: usize = 2;
    const TAG_COUNT: usize = 1 << Self::SHIFT;
    const MASK: usize = Self::TAG_COUNT - 1;

    const EXT_SHIFT: usize = 4;
    const EXT_MASK: usize = (1 << Self::EXT_SHIFT) - 1;

    pub const ZERO: Self = Self(0 << Self::SHIFT | Tag::Fixnum as usize);

    pub const TRUE: Self = Self(1 << Self::EXT_SHIFT | Tag::Bool as usize);
    pub const FALSE: Self = Self(0 << Self::EXT_SHIFT | Tag::Bool as usize);
    pub const NIL: Self = Self(0 << Self::EXT_SHIFT | Tag::Singleton as usize); // '()
    pub const UNBOUND: Self = Self(1 << Self::EXT_SHIFT | Tag::Singleton as usize); // 'tombstone'
    pub const UNSPECIFIED: Self = Self(2 << Self::EXT_SHIFT | Tag::Singleton as usize); // for 'unspecified' stuff
    pub const EOF: Self = Self(3 << Self::EXT_SHIFT | Tag::Singleton as usize); // #!eof

    const BOUNDS_SHIFT: usize = 8 * size_of::<Self>() - Self::SHIFT; // 30/62

    fn base_tag(self) -> BaseTag { unsafe { transmute((self.0 & Self::MASK) as u8) } }

    fn tag(self) -> Tag {
        let base_tag = self.base_tag();
        if BaseTag::Ext == base_tag {
            unsafe { transmute((self.0 & Self::EXT_MASK) as u8) }
        } else {
            unsafe { transmute(base_tag) }
        }
    }

    pub fn is_oref(self) -> bool { self.base_tag() == BaseTag::ORef }

    pub fn unpack(self) -> UnpackedValue {
        match self.tag() {
            Tag::ORef => UnpackedValue::ORef(HeapValue {value: self, _phantom: PhantomData}),
            Tag::Fixnum => UnpackedValue::Fixnum((self.0 >> Self::SHIFT) as isize),
            Tag::Flonum => unimplemented!(),
            Tag::Char => UnpackedValue::Char(unsafe { char::from_u32_unchecked((self.0 >> Self::EXT_SHIFT) as u32) }),
            Tag::Bool => UnpackedValue::Bool((self.0 >> Self::EXT_SHIFT) != 0),
            Tag::Singleton => match self {
                Self::NIL => UnpackedValue::Nil,
                Self::UNBOUND => UnpackedValue::Unbound,
                Self::UNSPECIFIED => UnpackedValue::Unspecified,
                _ => unimplemented!()
            }
        }
    }

    pub fn identity_hash(self) -> usize {
        if let Ok(oref) = HeapValue::<()>::try_from(self) {
            oref.identity_hash()
        } else {
            let mut hasher = DefaultHasher::default();
            self.0.hash(&mut hasher);
            hasher.finish() as usize
        }
    }
}

impl From<u16> for Value {
    fn from(n: u16) -> Self { Self((n as usize) << Self::SHIFT | BaseTag::Fixnum as usize) }
}

impl From<i16> for Value {
    fn from(n: i16) -> Self { Self((n as usize) << Self::SHIFT | BaseTag::Fixnum as usize) }
}

impl TryFrom<isize> for Value {
    type Error = (); // FIXME

    fn try_from(n: isize) -> Result<Self, Self::Error> {
        if n >> Self::BOUNDS_SHIFT == 0
           || n >> Self::BOUNDS_SHIFT == !0 as isize // fits in 30/62 bits, OPTIMIZE
        {
            Ok(Self((n << Self::SHIFT) as usize | BaseTag::Fixnum as usize))
        } else {
            Err(())
        }
    }
}

impl TryInto<isize> for Value {
    type Error = (); // FIXME

    fn try_into(self) -> Result<isize, Self::Error> {
        if self.base_tag() == BaseTag::Fixnum {
            Ok(self.0 as isize >> Self::SHIFT)
        } else {
            Err(())
        }
    }
}

impl TryFrom<usize> for Value {
    type Error = (); // FIXME

    fn try_from(n: usize) -> Result<Self, Self::Error> {
        if n >> Self::BOUNDS_SHIFT == 0 { // fits in 30/62 bits, OPTIMIZE
            Ok(Self(n << Self::SHIFT | BaseTag::Fixnum as usize))
        } else {
            Err(())
        }
    }
}

impl TryInto<usize> for Value {
    type Error = (); // FIXME

    fn try_into(self) -> Result<usize, Self::Error> {
        if self.base_tag() == BaseTag::Fixnum {
            Ok(self.0 >> Self::SHIFT)
        } else {
            Err(())
        }
    }
}

impl TryFrom<fsize> for Value {
    type Error = (); // FIXME

    fn try_from(n: fsize) -> Result<Self, Self::Error> {
        if unimplemented!() { // fits in 30/62 bits
            Ok(Self(unsafe { mem::transmute::<_, usize>(n) } << Self::SHIFT | BaseTag::Flonum as usize))
        } else {
            Err(())
        }
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self { Self((c as usize) << Self::EXT_SHIFT | Tag::Char as usize) }
}

impl TryInto<char> for Value {
    type Error = (); // FIXME

    fn try_into(self) -> Result<char, Self::Error> {
        if self.0 & Self::EXT_MASK == Tag::Char as usize {
            Ok(unsafe { char::from_u32_unchecked((self.0 >> Self::EXT_SHIFT) as u32) })
        } else {
            Err(())
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self { Self((b as usize) << Self::EXT_SHIFT | Tag::Bool as usize) }
}

impl TryInto<bool> for Value {
    type Error = (); // FIXME

    fn try_into(self) -> Result<bool, Self::Error> {
        if self.0 & Self::EXT_MASK == Tag::Bool as usize {
            Ok(self.0 >> Self::EXT_SHIFT != 0)
        } else {
            Err(())
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use UnpackedValue::*;

        match self.unpack() {
            ORef(v) => v.fmt(f),
            Fixnum(n) => n.fmt(f), // HACK
            Flonum(n) => unimplemented!(),
            Char(c) => c.fmt(f), // HACK
            Bool(true) => "#true".fmt(f),
            Bool(false) => "#false".fmt(f),
            Nil => "()".fmt(f),
            Unbound => "#<unbound>".fmt(f), // although we should never actually get here
            Unspecified => "#<unspecified>".fmt(f),
            _ => unimplemented!()
        }
    }
}

// ---

pub struct HeapValue<T: ?Sized> {
    pub value: Value,
    pub _phantom: PhantomData<*mut T>
}

impl<T: ?Sized> Clone for HeapValue<T> {
    fn clone(&self) -> Self { Self {value: self.value, _phantom: self._phantom} }
}

impl<T: ?Sized> Copy for HeapValue<T> {}

impl<T: ?Sized> HeapValue<T> {
    pub fn heap_tag(self) -> HeapTag { unsafe { (*self.as_ptr()).tag() } }

    pub fn as_ptr(self) -> *mut Object {
        unsafe { (self.data() as *mut Object).offset(-1) }
    }

    pub fn data(self) -> *mut u8 { (self.value.0 & !Value::MASK) as *mut u8 }

    pub fn identity_hash(self) -> usize { unsafe { (*self.as_ptr()).identity_hash() } }
}

impl HeapValue<()> {
    pub fn unpack(self) -> UnpackedHeapValue {
        match self.heap_tag() {
            HeapTag::Vector => UnpackedHeapValue::Vector(HeapValue {value: self.value, _phantom: PhantomData}),
            HeapTag::String => UnpackedHeapValue::String(HeapValue {value: self.value, _phantom: PhantomData}),
            HeapTag::Symbol => UnpackedHeapValue::Symbol(HeapValue {value: self.value, _phantom: PhantomData}),
            HeapTag::Pair => UnpackedHeapValue::Pair(HeapValue {value: self.value, _phantom: PhantomData}),
            HeapTag::Bindings => UnpackedHeapValue::Bindings(HeapValue {value: self.value, _phantom: PhantomData}),
            HeapTag::Closure => UnpackedHeapValue::Closure(HeapValue {value: self.value, _phantom: PhantomData})
        }
    }
}

impl<T> Deref for HeapValue<T> {
    type Target = T;
    
    fn deref(&self) -> &Self::Target { unsafe{ &*(self.data() as *const T) } }
}

impl<T> DerefMut for HeapValue<T> {
    fn deref_mut(&mut self) -> &mut Self::Target { unsafe { &mut*(self.data() as *mut T) } }
}

impl<T: ?Sized> From<HeapValue<T>> for Value {
    fn from(v: HeapValue<T>) -> Self { v.value }
}

impl TryFrom<Value> for HeapValue<()> {
    type Error = (); // FIXME

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.base_tag() == BaseTag::ORef {
            Ok(Self {value, _phantom: PhantomData})
        } else {
            Err(())
        }
    }
}

impl<T: Heaped + ?Sized> TryFrom<Value> for HeapValue<T> {
    type Error = (); // FIXME

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Ok(oref) = HeapValue::<()>::try_from(value) {
            if oref.heap_tag() == T::TAG {
                Ok(HeapValue {value: oref.value, _phantom: PhantomData})
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }
}

impl Display for HeapValue<()> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.unpack() {
            UnpackedHeapValue::Vector(vec) => {
                "#(".fmt(f)?;

                for (i, v) in vec.iter().enumerate() {
                    if i > 0 {
                        " ".fmt(f)?;
                    }
                    v.fmt(f)?;
                }

                ")".fmt(f)
            },
            UnpackedHeapValue::String(s) => s.fmt(f),
            UnpackedHeapValue::Symbol(s) => s.fmt(f),
            UnpackedHeapValue::Pair(mut p) => {
                "(".fmt(f)?;

                loop {
                    p.car.fmt(f)?;

                    if let Ok(cdr) = Pair::try_from(p.cdr) {
                        " ".fmt(f)?;
                        p = cdr;
                    } else {
                        break;
                    }
                }

                if p.cdr != Value::NIL {
                    write!(f, " . {}", p.cdr)?;
                }

                ")".fmt(f)
            },
            UnpackedHeapValue::Bindings(_) => "#<environment>".fmt(f),
            UnpackedHeapValue::Closure(_) => "#<procedure>".fmt(f)
        }
    }
}

// ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char() {
        let c = 'a';

        let v = Value::from(c);

        assert!(!v.is_oref());
        assert_eq!(c, v.try_into().unwrap());
    }

    #[test]
    fn test_bool() {
        let b = true;

        let v = Value::from(b);

        assert!(!v.is_oref());
        assert_eq!(b, v.try_into().unwrap());
    }

    #[test]
    fn test_fixnum() {
        let n = 23isize;

        let v = Value::from(23i16);

        assert!(!v.is_oref());
        assert_eq!(n, v.try_into().unwrap());

        let m = -n;

        let u = Value::try_from(m).unwrap();

        assert!(!u.is_oref());
        assert_eq!(m, u.try_into().unwrap());
    }
}


use std::alloc::{self, Layout};
use std::convert::TryFrom;
use std::marker::PhantomData;
use std::mem::{size_of, align_of, swap, transmute};
use std::ptr;

// ---

pub trait HeapObject: Copy {
    type Ref: ObjectReference<Object=Self>;
    // type Header: ObjectHeader<Ref=Self::Ref>;
    type Fields: Iterator<Item=*mut Self::Ref>;

    fn is_alignment_hole(mem: *const Self) -> bool;

    unsafe fn forwarding(oref: Self::Ref) -> Self;
    fn forward(&self) -> Option<Self::Ref>;

    fn size(&self) -> usize;
    fn align(&self) -> usize;

    fn data(&mut self) -> *mut u8;
    fn ptr_fields(&mut self) -> Self::Fields;
}

pub trait ObjectReference: Copy {
    type Object;

    unsafe fn from_ptr(ptr: *mut Self::Object) -> Self;
    fn as_mut_ptr(self) -> Option<*mut Self::Object>;
}

// ---

struct Semispace<O: HeapObject> {
    start: *mut u8,
    end: *mut u8,
    phantom: PhantomData<*const O>
}

impl<O: HeapObject> Semispace<O> {
    fn new(size: usize) -> Self {
        let layout = Layout::from_size_align(size, 1).unwrap();
        let start = unsafe { alloc::alloc(layout) };
        let end = unsafe { start.add(size) }; // safe since alloc::alloc succeeded
        Self {start, end, phantom: PhantomData}
    }
}

// FIXME: impl Drop

// ---

struct MemoryManager<O: HeapObject> {
    max_heap: usize,
    fromspace: Semispace<O>,
    tospace: Semispace<O>,
    free: *mut u8,
    grey: *mut u8
}

impl<O: HeapObject> MemoryManager<O> {
    fn new(initial_heap: usize, max_heap: usize) -> Self {
        debug_assert!(initial_heap <= max_heap);
        let fromspace = Semispace::new(initial_heap / 2);
        let tospace = Semispace::new(initial_heap / 2);
        let free = tospace.start;
        Self {max_heap, fromspace, tospace, free, grey: ptr::null_mut()}
    }

    fn alloc(&mut self, base: O) -> Option<O::Ref> {
        let align = base.align().max(align_of::<O>()); // sic

        let mut free = self.free as usize;

        let mut data = free.checked_add(size_of::<O>())?;
        data = data.checked_add(align - 1)? & !(align - 1);
        free = data.checked_add(base.size())?;

        if free <= self.tospace.end as usize {
            unsafe { ptr::write_bytes(self.free, 0, free - self.free as usize) };
            self.free = free as *mut u8;
            let obj = (data - size_of::<O>()) as *mut O;
            unsafe { *obj = base; }
            Some(unsafe { O::Ref::from_ptr(obj) })
        } else {
            None
        }
    }

    // TODO: Heap expansion logic
    unsafe fn collect_garbage(&mut self, roots: &[*mut O::Ref]) {
        // Swap semispaces:
        swap(&mut self.fromspace, &mut self.tospace);
        self.free = self.tospace.start;
        self.grey = self.tospace.start;

        // Trace roots:
        for &root in roots {
            *root = self.mark(*root);
        }

        // Trace the rest:
        while self.grey < self.free {
            self.find_header();
            self.scan(&mut *(self.grey as *mut O));
        }
    }

    fn mark(&mut self, oref: O::Ref) -> O::Ref {
        match oref.as_mut_ptr() {
            Some(obj) => {
                let obj = unsafe { &mut *obj };

                match obj.forward() {
                    Some(res) => res,
                    None => {
                        let res = self.alloc(*obj).unwrap(); // tospace is at least as big as fromspace
                        let new_obj = unsafe { &mut *res.as_mut_ptr().unwrap() }; // surely it is a pointer, we just allocated it
                        unsafe { ptr::copy_nonoverlapping(obj.data(), new_obj.data(), obj.size()); }
                        *new_obj = unsafe { O::forwarding(res) };
                        res
                    }
                }
            },
            None => oref
        }
    }

    fn find_header(&mut self) {
        let align = align_of::<O>();
        let grey = (self.grey as usize) + (align - 1) & !(align - 1);
        let mut grey = grey as *const O;
        
        while (grey as *const u8) < self.free {
            if O::is_alignment_hole(grey) {
                grey = unsafe { grey.add(1) };
            } else {
                self.grey = grey as *mut u8;
                return;
            }
        }
    }

    fn scan(&mut self, obj: &mut O) {
        for field in obj.ptr_fields() {
            unsafe {
                *field = self.mark(*field);
            }
        }
        self.grey = unsafe { self.grey.add(size_of::<O>() + obj.size()) };
    }
}

// FIXME: impl Drop

// ---

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter;

    #[derive(Clone, Copy)]
    struct Hdr(usize);

    impl Hdr {
        const SHIFT: usize = 2;
        const MASK: usize = (1 << Self::SHIFT) - 1;
        const HDR_TAG: usize = 0b01;
        const FWD_TAG: usize = 0b11;

        fn from_size(size: usize) -> Self { Self(size << Self::SHIFT | Self::HDR_TAG) }

        fn is_alignment_hole(mem: *const Self) -> bool { unsafe { (*mem).0 == 0 } }

        unsafe fn forwarding(ptr: *mut u8) -> Self { Self(ptr as usize | Self::FWD_TAG) }

        fn forward(&self) -> Option<*mut u8> {
            if self.0 & Self::MASK == Self::FWD_TAG {
                Some((self.0 & !Self::MASK) as *mut u8)
            } else {
                None
            }
        }

        fn size(&self) -> usize { self.0 >> Self::SHIFT }
        fn align(&self) -> usize { 8 }
    }

    #[derive(Clone, Copy)]
    pub struct Obj {
        header: Hdr
    }

    impl HeapObject for Obj {
        type Ref = *mut u8;
        type Fields = iter::Empty<*mut Self::Ref>;

        fn is_alignment_hole(mem: *const Self) -> bool { Hdr::is_alignment_hole(unsafe { &(*mem).header }) }

        unsafe fn forwarding(oref: Self::Ref) -> Self { Self {header: Hdr::forwarding(oref)} }
        fn forward(&self) -> Option<Self::Ref> { self.header.forward() }

        fn size(&self) -> usize { self.header.size() }
        fn align(&self) -> usize { self.header.align() }

        fn data(&mut self) -> *mut u8 { unsafe { (self as *mut Self).add(1) as *mut u8 } }
        fn ptr_fields(&mut self) -> Self::Fields { iter::empty() }
    }

    impl ObjectReference for *mut u8 {
        type Object = Obj;

        unsafe fn from_ptr(ptr: *mut Self::Object) -> Self { ptr.add(1) as *mut u8 }
        fn as_mut_ptr(self) -> Option<*mut Self::Object> { Some(unsafe { (self as *mut Self::Object).offset(-1) }) }
    }

    #[test]
    fn test_alloc() {
        let mut heap: MemoryManager<Obj> = MemoryManager::new(4 << 10, 1 << 20);
        let obj = Obj {header: Hdr::from_size(32)};
        assert!(heap.alloc(obj).is_some());
    }

    #[test]
    fn test_collect() {
        let mut heap: MemoryManager<Obj> = MemoryManager::new(4 << 10, 1 << 20);
        let obj = Obj {header: Hdr::from_size(32)};

        for _ in 0..10 {
            assert!(heap.alloc(obj).is_some());
        }

        let mut roots = [heap.alloc(obj).unwrap(), heap.alloc(obj).unwrap()];
        unsafe { heap.collect_garbage(&[&mut roots[0], &mut roots[1]]) };
    }
}


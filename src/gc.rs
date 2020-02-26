use std::alloc::{self, Layout};
use std::convert::TryFrom;
use std::marker::PhantomData;
use std::mem::{size_of, align_of, swap, transmute};
use std::ptr;

// ---

pub trait HeapObject {
    type Ref: ObjectReference;
    type Header: ObjectHeader<Ref=Self::Ref>;
    type Fields: Iterator<Item=*mut Self::Ref>;

    fn ptr_fields(header: &mut Self::Header) -> Self::Fields;
}

pub trait ObjectReference: Copy {
    unsafe fn from_ptr(ptr: *mut u8) -> Self;
    fn as_mut_ptr(self) -> Option<*mut u8>;
}

pub trait ObjectHeader: Clone + TryFrom<usize> { // FIXME: TryFrom is too powerful
    type Ref: ObjectReference;

    unsafe fn forwarding(ptr: *mut u8) -> Self;
    fn forward(&self) -> Option<Self::Ref>;
    fn size(&self) -> usize;
    fn align(&self) -> usize;
}

struct Object<O: HeapObject> {
    header: O::Header
}

impl<O: HeapObject> Object<O> {
    fn ptr_fields(&mut self) -> O::Fields { O::ptr_fields(&mut self.header) }
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

    fn alloc(&mut self, header: &O::Header) -> Option<O::Ref> {
        let align = header.align().max(align_of::<O::Header>()); // sic

        let mut free = self.free as usize;

        let mut data = free.checked_add(size_of::<O::Header>())?;
        data = data.checked_add(align - 1)? & !(align - 1);
        free = data.checked_add(header.size())?;

        if free <= self.tospace.end as usize {
            unsafe { ptr::write_bytes(self.free, 0, free - self.free as usize) };
            self.free = free as *mut u8;
            let header_mem = (data - size_of::<O::Header>()) as *mut O::Header;
            unsafe { *header_mem = header.clone(); }
            Some(unsafe { O::Ref::from_ptr(data as *mut u8) })
        } else {
            None
        }
    }

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
            self.scan(&mut *(self.grey as *mut Object<O>));
        }
    }

    fn mark(&mut self, oref: O::Ref) -> O::Ref {
        match oref.as_mut_ptr() {
            Some(data) => {
                let header = unsafe { transmute::<_, &mut O::Header>((data as *mut O::Header).offset(-1)) };

                match header.forward() {
                    Some(res) => res,
                    None => {
                        let res = self.alloc(&*header).unwrap(); // tospace is at least as big as fromspace
                        let new_data = res.as_mut_ptr().unwrap(); // surely it is a pointer, we just allocated it
                        unsafe { ptr::copy_nonoverlapping(data, new_data, header.size()); }
                        *header = unsafe { O::Header::forwarding(new_data) };
                        res
                    }
                }
            },
            None => oref
        }
    }

    fn find_header(&mut self) {
        let align = align_of::<O::Header>();
        let grey = (self.grey as usize) + (align - 1) & !(align - 1);
        let mut grey = grey as *const O::Header;
        
        while (grey as *const u8) < self.free {
            if O::Header::try_from(unsafe { *(grey as *const usize) }).is_ok() {
                self.grey = grey as *mut u8;
                return;
            } else {
                grey = unsafe { grey.add(1) };
            }
        }
    }

    fn scan(&mut self, obj: &mut Object<O>) {
        for field in obj.ptr_fields() {
            unsafe {
                *field = self.mark(*field);
            }
        }
        self.grey = unsafe { self.grey.add(size_of::<O::Header>() + obj.header.size()) };
    }
}

// ---

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter;

    struct Obj;

    impl HeapObject for Obj {
        type Ref = *mut u8;
        type Header = Hdr;
        type Fields = iter::Empty<*mut Self::Ref>;

        fn ptr_fields(header: &mut Hdr) -> Self::Fields { iter::empty() }
    }

    #[derive(Clone)]
    struct Hdr(usize);

    impl TryFrom<usize> for Hdr {
        type Error = ();

        fn try_from(size: usize) -> Result<Self, Self::Error> {
            if size != 0 {
                Ok(Self(size))
            } else {
                Err(())
            }
        }
    }

    impl ObjectHeader for Hdr {
        type Ref = *mut u8;

        unsafe fn forwarding(ptr: *mut u8) -> Self { Self(ptr as usize) }

        fn forward(&self) -> Option<Self::Ref> {
            if self.0 & 3 == 0 {
                Some(self.0 as *mut u8)
            } else {
                None
            }
        }

        fn size(&self) -> usize { self.0 >> 2 }
        fn align(&self) -> usize { 8 }
    }

    impl ObjectReference for *mut u8 {
        unsafe fn from_ptr(ptr: *mut u8) -> Self { ptr }
        fn as_mut_ptr(self) -> Option<*mut u8> { Some(self) }
    }

    #[test]
    fn test_alloc() {
        let mut heap: MemoryManager<Obj> = MemoryManager::new(4 << 10, 1 << 20);
        let header = Hdr::try_from(32 << 2 | 1).unwrap();
        assert!(heap.alloc(&header).is_some());
    }

    #[test]
    fn test_collect() {
        let mut heap: MemoryManager<Obj> = MemoryManager::new(4 << 10, 1 << 20);
        let header = Hdr::try_from(32 << 2 | 1).unwrap();

        for _ in 0..10 {
            assert!(heap.alloc(&header).is_some());
        }

        let mut roots = [heap.alloc(&header).unwrap(), heap.alloc(&header).unwrap()];
        unsafe { heap.collect_garbage(&[&mut roots[0], &mut roots[1]]) };
    }
}


Lofty Aspirations
=================

Bytecode
--------

* Register machine, similar to Lua(jit)
* Use LuaJIT/Chez "zero-cost frame links" trick.
* Compiler in Scheme
* No unsafe instructions
* Code object consists of an ``u32vector`` of bytecodes, a constant pool vector
  and debug information
* Compiler does closure conversion and stack-compacting register allocation for
  space safety (and efficiency)

Jit
---

* Self-style optimizing method jit
    - AFAIK has not been done, while Racket and Guile have template jits and
      also experimental tracing jits (Pycket and Nash respectively).
* Add native code pointer to bytecode objects. Bytecode interpreter calls that
  code pointer. If there is no jitted code, the callee just returns immediately
  and the call is interpreted.
* jitted code *jumps* to code pointer. If there is no jitted code, the return
  returns to the interpreter (i.e. even in jit mode the Scheme stack and C
  stack have nothing to do with each other). "Forwards" deoptimization is that
  simple, kind of like a tracing jit. Inlined loops need to check each
  iteration that the code pointer of the inliner has not changed and if it has,
  call the closure of the loop instead.
* Before jitted code makes the call jump it pushes the native return address
  to the stack. We align the destination so that the return address looks like
  a fixnum. The GC then ignores it, which does not matter since the bytecode ip
  indirectly keeps the native code object alive. Because of the extra push the
  jitted code expects to get return values shifted wrt. the bytecode mode.
* When bytecode returns, it looks at the top of the previous frame. If it
  finds a bytecode object the return works as before the jit existed. If it
  finds a native return address it calls [sic] the return address.
* When jitted code returns it also looks at the previous frame top. If it finds
  a native code address it just jumps there, otherwise it returns to the
  interpreter.
* The previous frame can also be deoptimized. In this case the return address
  points to another inverse trampoline. Here the return values have to be
  shifted down over the return address since that is how the bytecode expects
  them. That is the only OSR we do, otherwise frame layout is the same (even
  with inlining).
* Native return addresses must be pushed in order to return to inlined jitted
  code. Otherwise the best we could do is run in a different function for every
  bytecode-level stack frame.
* When native code becomes available, it is patched to the bytecode object code
  pointer slot, to wait for next call. At least at first we make no attempt to
  patch it into stack frames, so there might be a long interpreted return
  sequence. Hopefully most performance sensitive code is tail recursive.
* Deoptimization patches the native return addresses to the OSR inverse
  trampoline in addition to the bytecode object code pointer slot. We also need
  to patch stack segments on the heap including ones in captured continuations.
  Let's hope this does not cause an unreasonable pause.
* Since we have various guards and a copying collector, native code needs to
  be traced and pointers relocated. Native code objects need a table for this,
  like in the Chez profiling paper.
* SSA/CPS (HotSpot/Thorin) IR with StarJIT style guard dependency optimization?
* In interpreted mode functions have a hotness counter and calls have PICs
  (with counters?).


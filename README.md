# Pegasos Scheme

> PEdacoGicAl Scheme Of Sorts

## Planned Features

* R7RS
* `syntax-case`
* Mutable toplevel and library bindings mode (for "REPL-driven development")
* Delimited continuations
* Indexed fields
* Simple multiple dispatch
* Embeddability in Rust (and C?)

## Lofty Perf Plans

* Register-based bytecode VM
* Polymorphic inline caches
* Optimizing method JIT

## Non-goals

* Full R6RS support
* Nontrivial finalizers
    - Freeing unmanaged memory is about the only thing finalizers are suitable
      for, and even there mostly as a leak prevention safety net
* Weak pointers
    - The GC can't manage your caches properly


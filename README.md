
Learning Rust by playing with the Common Expression Language.



## Plan


- Write a very basic CEL parser in Rust (using Chumsky)
- Evaluate the Chumsky AST by implementing a basic Treewalk interpreter


### Extensions

- Possibly write a bytecode compiler for a simple stack based VM...  
- Research and try the existing Rust implementations of CEL.
- Benchmark the Cloud Custodian Python-CEL implementation against the Rust ones.
- If performance is significantly better, consider wrapping an existing Rust one for Python.


## Existing Implementations

- [clarkmcc/cel-rust](https://github.com/clarkmcc/cel-rust) (Maintained fork of https://github.com/clarkmcc/cel-rust)
  Implements separate crates for parser (using lalrpop) and interpreter (straightforward treewalk interpreter).
- [https://github.com/thesayyn/cel-rust](thesayyn/cel-rust) (Incomplete). WASM target with online [demo](https://thesayyn.github.io/cel-rust/).
  Parser uses lalrpop.
  
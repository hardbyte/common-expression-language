
Learning Rust by playing with the [Common Expression Language](https://github.com/google/cel-spec).



## Plan


- Write a very basic CEL Lexer/Parser in Rust (using [Chumsky](https://crates.io/crates/chumsky))
- Evaluate the parsed CEL AST by implementing a basic Treewalk interpreter
- Write a basic CLI for evaluating CEL expressions with data loaded from stdin or a JSON/YAML file.

### Extensions

- Use a Pratt parser once Chumsky supports it (https://github.com/zesterer/chumsky/pull/380)
- Possibly write a bytecode compiler for a simple stack based VM...
- Research and try the existing Rust implementations of CEL.
- Benchmark the Cloud Custodian Python-CEL implementation against the Rust ones.
- If performance is significantly better, consider wrapping an existing Rust one for Python.
- Use an existing Lexer like [Logos](https://docs.rs/logos/latest/logos/)?

## Existing Implementations

- [clarkmcc/cel-rust](https://github.com/clarkmcc/cel-rust) (Maintained fork of [orf/cel-rust](https://github.com/orf/cel-rust).)
  Implements separate crates for parser (using lalrpop) and interpreter (straightforward treewalk interpreter).
- [thesayyn/cel-rust](https://github.com/thesayyn/cel-rust) (Incomplete). WASM target with online [demo](https://thesayyn.github.io/cel-rust/).
  Parser uses lalrpop.

Test Data

https://github.com/google/cel-spec/blob/master/tests/simple/testdata/basic.textproto

## Execution

```
cargo run -- --expression="-(3.0 + 5.0) * 2.0e0 - -1.0"
```
Which outputs something like:
```
AST:
Binary(Binary(Unary(Neg, Binary(Atom(Double(3.0)), Add, Atom(Double(5.0)))), Mul, Atom(Double(2.0))), Sub, Unary(Neg, Atom(Double(1.0))))

Evaluating program
NumericCelType(Float(-15.0))
```

Note: haven't implemented any type coercing, or much of the language beyond number literals yet!

## References

Real parsers using Chumsky:
- [jaq](https://github.com/01mf02/jaq/blob/main/jaq-parse/src/token.rs)
- https://github.com/panda-re/panda/blob/dev/panda/plugins/cosi_strace/src/c_type_parser.rs
- [prql](https://github.com/PRQL/prql/blob/main/prql-compiler/src/parser) (uses a separate Chumsky Lexer to create Vec<Token>, then parses Token stream into Expr, Stmt, Literal etc)
- [flux](https://github.com/fluxed-lang/flux/blob/main/crates/compiler/fluxc_parser/src/lib.rs) - lexed with [Logos](https://github.com/fluxed-lang/flux/blob/main/crates/compiler/fluxc_lexer/src/lib.rs) then parsed with Chumsky.
- [Cotton](https://github.com/nanikamado/cotton/blob/main/compiler/parser/src/parse.rs) Separate Chumsky Lexer and Parser.
- [Cornucopia](https://github.com/cornucopia-rs/cornucopia/blob/main/crates/cornucopia/src/parser.rs) parses SQL.
- [percival](https://github.com/ekzhang/percival/blob/main/crates/percival/src/parser.rs) nice split between Lexer and Parser with WASM target.
- [monty](https://github.com/mental32/monty/blob/master/montyc_parser/src/comb.rs) uses a lot of Rust macros
- [blackstone](https://github.com/BlackstoneDF/blackstone/blob/main/src/parser/parse.rs)
- [savage](https://github.com/p-e-w/savage/blob/master/savage_core/src/parse.rs)


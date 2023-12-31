
Learning Rust by playing with the [Common Expression Language](https://github.com/google/cel-spec).

## Plan

- Write a very basic CEL Lexer/Parser in Rust (using [Chumsky](https://crates.io/crates/chumsky))
- Evaluate the parsed CEL AST by implementing a basic Treewalk interpreter
- Write a basic CLI for evaluating CEL expressions with data loaded from stdin or a JSON/YAML file.
- Create rust packages for the Lexer/Parser and Interpreter
- See how conformant we can get to the CEL spec, parse the test data and evaluate the expressions.
- Wrap for Python

### Extensions

- Use a Pratt parser ~once~ now that Chumsky supports it (https://github.com/zesterer/chumsky/pull/464)
- Possibly write a bytecode compiler for a simple stack based VM...
- Research and try the existing Rust implementations of CEL.
- Benchmark the Cloud Custodian Python-CEL implementation against the Rust ones.
- Use an existing Lexer like [Logos](https://docs.rs/logos/latest/logos/)


## Execution

```
cargo run -- --expression="-(3.0 + 5.0) * 2.0e0 - -1.0"
```
Which outputs something like:
```
AST:
Arithmetic(Arithmetic(Unary(Neg, Arithmetic(Atom(Float(3.0)), Add, Atom(Float(5.0)))), Multiply, Atom(Float(2.0))), Subtract, Unary(Neg, Atom(Float(1.0))))

Evaluating program
NumericCelType(Float(-15.0))
```

Note while the parser is close to complete I'm not intending to implement a full interpreter
with all built-in functions.

The interpreter has basic support for strings, numbers, bools, lists, maps, variables, and
binary and unary operators.


```shell
cargo run -- --expression="size('hello world') > 5u"
```

Outputs:
```
AST:
Relation(Member(Ident("size"), FunctionCall([Atom(String("hello world"))])), GreaterThan, Atom(UInt(5)))

Evaluating program
Bool(true)
```
### Context

To add context from a JSON file use `--input` with either a filename or `-` to read from
stdin:

```shell
echo "{\"foo\":\"bar\"}" | cargo run -- --expression="foo" --input -
```

Outputs:

```
Parsed context data: Object {"foo": String("bar")}
AST:
Ident("foo")

Evaluating program
String("bar")
```


### Python Bindings

A Python package `rustycel` using PyO3 in `python_bindings` offers a simple `evaluate` function from Python:

```python
>>> import rustycel
>>> rustycel.evaluate("1u + 4u")
5
```

_Note only a few primitive types are mapped back to Python native types._


# References

## Existing CEL Implementations

- [cel-go](https://github.com/google/cel-go) - Reference implementation of CEL by Google in Go.
- [cel-python](https://github.com/cloud-custodian/cel-python) - Python implementation of CEL by Cloud Custodian
  as used in [Netchecks](https://github.com/hardbyte/netchecks).
- [clarkmcc/cel-rust](https://github.com/clarkmcc/cel-rust) (Maintained fork of [orf/cel-rust](https://github.com/orf/cel-rust).)
  Implements separate crates for parser (using lalrpop) and interpreter (straightforward treewalk interpreter).
- [thesayyn/cel-rust](https://github.com/thesayyn/cel-rust) (Incomplete). WASM target with online [demo](https://thesayyn.github.io/cel-rust/).
  Parser uses lalrpop.

Test Data

https://github.com/google/cel-spec/blob/master/tests/simple/testdata/basic.textproto

## Real parsers using Chumsky

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

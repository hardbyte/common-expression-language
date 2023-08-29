
A POC of Python bindings.

```
maturin develop
```

Then in Python you can import `rustycel` and call `evaluate` with your
CEL expression:

```python
>>> import rustycel
>>> rustycel.evaluate("1u + 4u")
5
>>> rustycel.evaluate("size('hello world') > 4")
True
>>> rustycel.evaluate("[1u , 4u]")
'RustyCelType(List([NumericCelType(UInt(1)), NumericCelType(UInt(4))]))'
```

Note only a few primitive types are mapped back to Python native types, falling
back to a debug representation of the CEL type (like the last example).

To take this further we would want to:

1. Converting any evaluated CEL result (`CelType`) into Python native objects.
1. Pass in Python native objects into CEL evaluation as the context.
1. Pass in Python functions that can be called from CEL expressions.
1. Could split the compile/evaluate steps into separate functions.

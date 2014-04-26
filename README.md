# Cauterize

## Schemas

### Types

#### BuiltIn

There are several types that represent the foundation of the Cauterize types.
These are the funadmental types available for creating your own more complex
types.

Integral types are assumed to be little endian on the wire.

Unsigned integer types:

  * `u8`
  * `u16`
  * `u32`
  * `u64`

Signed values are assumed to be two's complement and little endian.

Signed integer types:

  * `s8`
  * `s16`
  * `s32`
  * `s64`

Booleans are always encoded as a single byte.

A single boolean type:

  * `bool`

Floating point types are hard. Their definitions can be different (or missing
entirely!) across CPU architectures. Therefor, we only define the two most
commonly used floating point representations. They both have unambiguous binary
represntation. The `ieee754s` is a single-precision IEEE 754 single-precision
32 bit floating point value. The `ieee754d` is is a double-precision IEEE 754
double-precision 64-bit floating point value.

Floating point types:

  * `ieee754s`
  * `ieee754d`

#### Constants

Constants are designed to allow the explicit placement of a literal value
inside of a type. These literal values are (currently) limited to being one of
the built-in types. In the future, it may be possible to include
user-defined-type literals in a schema, but not yet.

Constants allow schemas to include explicit values.

The general pattern is as follows:

```scheme
(const [built-in type] [value])
```

This allows us to express literal values like the following:

```scheme
(const specialNumber u64 4993)
```

When constants are written into the serialization stream, they are represented
as the specified built-in type would have been specified, but with the
prescribed value.

## Specifications

```scheme
(name "cauterize")
(version "0.1.0")
(hash "22596363b3de40b06f981fb85d82312e8c0ed511")
```

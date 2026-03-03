`safe-literals` is a GHC plugin that rewrites your programs such that you get a type error
whenever you use a literal that doesn't fit the target type. It works in any context, mono-
or polymorphic. It mostly makes sense in context of custom number types, such as
[clash-lang](https://clash-lang.org/)'s`Unsigned`, `Signed`, and `Index`.

- [How to use](#how-to-use)
- [How it works](#how-it-works)
  - [Examples](#examples)
    - [Out-of-bound, positive literal in monomorphic context](#out-of-bound-positive-literal-in-monomorphic-context)
    - [Out-of-bound, negative literal in monomorphic context](#out-of-bound-negative-literal-in-monomorphic-context)
    - [Polymorphic context](#polymorphic-context)
    - [Polymorphic context with Clash types](#polymorphic-context-with-clash-types)
- [FAQ](#faq)
  - [Why not rely on GHC's builtin warnings?](#why-not-rely-on-ghcs-builtin-warnings)
  - [Couldn't you only insert for types you recognize?](#couldnt-you-only-insert-for-types-you-recognize)
  - [Couldn't you write this as a core-to-core plugin?](#couldnt-you-write-this-as-a-core-to-core-plugin)
  - [Couldn't you write this as a type-checker plugin?](#couldnt-you-write-this-as-a-type-checker-plugin)
  - [Why not a warning?](#why-not-a-warning)
  - [What about `Float`/`Double`?](#what-about-floatdouble)
  - [What about rational literals (e.g., `3.1415`)?](#what-about-rational-literals-eg-31415)

# How to use
Add `safe-literals` to your library's `build-depends` and `-fplugin=SafeLiterals` to its
`ghc-options`, like this:

```yaml
library
  [..]

  build-depends:
    [..]
    safe-literals

  ghc-options: -fplugin=SafeLiterals
```


# How it works

## Integer Literals
Every positive integer literal is rewritten as `safePositiveIntegerLiteral @lit lit` and every
negative integer literal is rewritten as `safeNegativeIntegerLiteral @lit (-lit)`. The `safe`
functions themselves act as `id`, but insert a `Safe{Positive,Negative}IntegerLiteral lit a`
constraint where `a` is the type of the literal (possibly polymorphic). Every instance of
this class should insert a constraint that's checkable by the type checkers. For example,
an instance of `Word8` might look like:

```haskell
instance (lit <= 255) => SafePositiveIntegerLiteral lit Word8
```

## Rational Literals
Rational literals (e.g., `3.14`) are rewritten as `safePositiveRationalLiteral @num @den lit`
where `num` and `den` are the numerator and denominator. Since GHC lacks type-level rationals,
we represent them as two type-level naturals. The plugin ensures:

- For `Float`/`Double`: all rational literals are accepted (but may lose precision)
- For integer types (`Int`, `Word8`, etc.): only integer-valued rationals are accepted (e.g., `2.0` works, `2.5` doesn't)
- For `Ratio a`: the numerator and denominator must fit in type `a`
- For fixed-point types: checks both range and precision (denominator must divide a power of 2)

In practice, these instances are a bit more complicated in order to have nice error type
errors.

## Examples
### Out-of-bound, positive literal in monomorphic context
```haskell
x :: Word8
x = 259
```

```haskell
error: [GHC-64725]
    • Literal 259 is out of bounds.
      Word8 has bounds: [0 .. 255].
      Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check.
    • In the expression: safePositiveIntegerLiteral @259 259
      In an equation for ‘exampleWord8’:
          x = safePositiveIntegerLiteral @259 259
  |
9 | x = 259
  |     ^^^
```

### Out-of-bound, negative literal in monomorphic context
```haskell
x :: Word8
x = -1
```

```haskell
error: [GHC-64725]
    • Negative literal -1 is out of bounds.
      Word8 has bounds: [0 .. 255].
      Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check.
    • In the expression: safeNegativeIntegerLiteral @1 -1
      In an equation for ‘x’:
          x = safeNegativeIntegerLiteral @1 -1
  |
9 | x = -1
  |     ^^
```

### Polymorphic context
```haskell
x :: Num a => a
x = -1
```

```
error: [GHC-39999]
    • Could not deduce ‘SafeNegativeIntegerLiteral 1 a’
        arising from a use of ‘safeNegativeIntegerLiteral’
      from the context: Num a
        bound by the type signature for:
                   x :: forall a. Num a => a
        at examples.hs:8:1-15
    • In the expression: safeNegativeIntegerLiteral @1 - 1
      In an equation for ‘x’: x = safeNegativeIntegerLiteral @1 - 1
  |
9 | x = -1
  |     ^^
```

### Polymorphic context with Clash types
```haskell
x :: (4 <= n, KnownNat n) => Unsigned n
x = 255
```

```
error: [GHC-64725]
    • Literal 255 is out of bounds.
      Unsigned n has bounds: [0 .. (2 ^ n) - 1].
      Possible fix: add '8 <= n' to the context.
      Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check.
    • In the expression: safePositiveIntegerLiteral @255 255
      In an equation for ‘x’: x = safePositiveIntegerLiteral @255 255
  |
9 | x = 255
  |     ^^^
```

# FAQ
## Why not rely on GHC's builtin warnings?
GHC's builtin warnings work fine for builtin types when they're monomorphic:

```haskell
ghci> x = -5 :: Word
<interactive>:1:6: warning: [GHC-97441] [-Woverflowed-literals]
    Literal -5 is out of the Word range 0..18446744073709551615
```

But it's easy to (accidentally) work around:

```haskell
ghci> x = -5 :: Num a => a
ghci> x :: Word
18446744073709551611
```

More importantly, it doesn't work with custom numeric types, such as Clash's `Signed`,
`Unsigned`, and `Index`.

## Couldn't you only insert for types you recognize?
Maybe, but you'd encounter the same issues as GHC's builtin system does. (See previous question.)

## Couldn't you write this as a core-to-core plugin?
You can't insert constraints anymore, as type checking has already run. Yes, you could access
types and write your own solvers, but this would balloon the size of the plugin. More
importantly, it would bypass GHC's usual type checking behavior and user plugins, which
is bound to cause issues where GHC would usually approve/reject constraints, but the plugin
doesn't.

## Couldn't you write this as a type-checker plugin?
Maybe in combination with other passes, but _just_ the type checkers don't have access to
term level literals.

## Why not a warning?
Because there is no `TypeWarning` :-).

## What about `Float`/`Double`?
`Float` and `Double` are now fully supported for rational literals (e.g., `3.14`). Note that:

- Integer literals (e.g., `42`) will still error for `Float`/`Double` - use rational notation (e.g., `42.0`) instead
- Rational literals may lose precision due to the limitations of floating-point representation
- The plugin will accept any rational literal for these types; precision loss is expected and not reported

## What about rational literals (e.g., `3.1415`)?
Rational literals are now fully supported! The plugin represents them using two type-level
naturals (numerator and denominator) since GHC lacks type-level rationals.

### Examples

```haskell
-- Float/Double: always accepted, may lose precision
x :: Double
x = 3.14159  -- ✓

-- Ratio: numerator and denominator must fit in the underlying type
y :: Ratio Integer
y = 22.7  -- ✓ (treated as 227 % 10)

-- Integer types: must be integer-valued
z :: Word8
z = 42.0  -- ✓
w :: Word8
w = 2.5   -- ✗ Error: "is not an integer"

-- Fixed-point types: checks range and exact representability
data UFixed (i :: Nat) (f :: Nat)  -- i integer bits, f fractional bits

v :: UFixed 8 8
v = 0.5   -- ✓ (0.5 = 1/2, representable with 8 fractional bits)
u :: UFixed 8 8
u = 0.333 -- ✗ Error: "cannot be represented exactly" (1/3 needs infinite bits)
```

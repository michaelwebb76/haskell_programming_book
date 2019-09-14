# Chapter 4

## Data declarations

```!haskell
data Bool = False | True
```

In this example:
* The whole statement is a "data declaration" or "datatype"
* `Bool` is a "type constructor"
* `False` and `True` are both "data constructors"

```!haskell
data (,) a b = (,) a b
```

In this example:
* This is a "product type" because both arguments are required to produce a result
* This is the underlying datatype of a Tuple

## Numeric types

`Int` = limited size integer

`Integer` = less-limited size integer (always use this one)

`Float` = single precision floating point number

`Double` = double precision floating point number (always use this one)

`Rational` = fractional number with numerator and denominator

`Scientific` = space efficient, arbitrary precision number type (from package)


# Chapter 6

## Typeclasses

```!haskell
instance Eq Trivial where
    (==) :: Trivial -> Trivial -> Bool
    (==) Trivial' Trivial' = True
```

This is a declaration of a typeclass instance (aka an instance of an
implementation of a typeclass, in this case `Eq` for a specific type,
in this case, `Trivial`).

```!haskell
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show
```

How to add one of the out-of-the-box typeclasses (no extra work
required). These typeclasses include `Eq`, `Show`, `Ord`, `Enum`,
`Bounded` and `Read` (but you should never use this last one).

```!haskell
class (Real a, Enum a) => Integral a where
```

This is the declaration of a typeclass that can only be applied to types that
implement `Real` and `Enum`.

# Chapter 9

*Higher order functions* are those functions that take functions as arguments.

When dealing with lists, the *spine* refers to all the `cons` operators
without referring to any of the actual data in the list.

```!haskell
 [x^2 | x <- [1..10], rem x 2 == 0]
 ```

 ... is a *list comprehension* that applies the function on the left to the
 list (generator) on the right. You can also optionally include predicates to filter the
 list.

 It is also possible to write a list comprehensions with multiple generators.

 ```!haskell
 [x^y, x <- [1..5], y <- [2, 3]]
 ```

# Chapter 10

Never use `foldl` on infinite lists

`foldr` evaluates right to left like `(0 + (1 + (2 + (3 + 4))))`
`foldl` evaluates left to right like `((((0 + 1) + 2) + 3) + 4)`

That's fine for associative operations like `+` and `*`, but for
others it results in completely different results.

# Chapter 11

Use `:k` in GHCI to see the kind of a type. If it is `*` the type
is fully applied.

The kind of `[]` is `* -> *` because it doesn't know what type
goes into the empty list yet.

With record types, each attribute declaration is actually declaring
a function that will enable you to extract that value from the record.
I imagine that this would create a lot of naming contention if you're
not careful.

You can use an as-pattern to refer to the full type while still pulling
out the constituent parts using pattern matching.

```!haskell
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t
```

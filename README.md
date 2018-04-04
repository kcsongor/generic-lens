# generic-lens

[![Build Status](https://travis-ci.org/kcsongor/generic-lens.svg?branch=master)](https://travis-ci.org/kcsongor/generic-lens)
[![Hackage](https://img.shields.io/hackage/v/generic-lens.svg)](https://hackage.haskell.org/package/generic-lens)

Generically derive traversals, lenses and prisms.

Available on [Hackage](https://hackage.haskell.org/package/generic-lens)

This library uses `GHC.Generics` to derive efficient optics (traversals, lenses
and prisms) for algebraic data types in a type-directed way, with a focus on
good type inference and error messages when possible.

This package uses the GHC 8 `Generic` representation to derive various operations
on data structures with lens interfaces, including structural subtype
relationships between records and positional indexing into arbitrary product
types.

The derived optics use the so-called van Laarhoven representation, thus are
fully interoperable with the combinators found in mainstream lens libraries.

Examples can be found in the `examples` and `tests` folders.

## Preliminaries
A typical module using `generic-lens` will usually have the following
extensions turned on:
```haskell
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
```

# Taxonomy of optics
Here is a comprehensive list of the optics exposed by `generic-lens`. The
combinators each allows a different way of identifying certain parts of
algebraic data types.

## Lenses

A lens identifies exactly one part of a product type, and allows querying and
updating it.

### By name

```haskell
data Person = Person { name :: String, age :: Int } deriving (Generic, Show)

sally :: Person
sally = Person "Sally" 25
```

Record fields can be accessed by their label using the `field` lens.

```haskell
>>> sally ^. field @"name"
"Sally"

>>> sally & field @"name" .~ "Tamas"
Person {name = "Tamas", age = 25}
```
Here we use [visible type application](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#visible-type-application)
to specify which field we're interested in, and use the `^.` and `.~` combinators from a lens library
([lens](https://hackage.haskell.org/package/lens), [microlens](https://hackage.haskell.org/package/microlens), etc.)
to query and update the field.

Or for standalone use, the `getField` and `setField` functions can be used instead.
```haskell
>>> getField @"age" sally
25

>>> setField @"age" 26 sally
Person {name = "Sally", age = 26}
```

When a non-existent field is requested, the library generates a helpful type error:
```haskell
>>> sally ^. field @"pet"
error:
  • The type Person does not contain a field named 'pet'
```

For types with multiple constructors, we can still use `field` as long as all constructors contain the required field
```haskell
data Two = First { wurble :: String, banana :: Int } | Second { wurble :: String } deriving (Generic, Show)

>>> Second "woops" ^. field @"wurble"
"woops"
>>> Second "woops" ^. field @"banana"
    ...
    • Not all constructors of the type Two
       contain a field named 'banana'.
      The offending constructors are:
      • Second
    ...
```

The type of `field` is
```haskell
field :: HasField name s t a b => Lens s t a b
```
Therefore it allows polymorphic (type-changing) updates, when the accessed field mentions type parameters.

```haskell
data Foo f a = Foo
  { foo :: f a
  } deriving (Generic, Show)

foo1 :: Foo Maybe Int
foo1 = Foo (Just 10)

-- |
-- >>> foo2
-- Foo {foo = ["10"]}
foo2 :: Foo [] String
foo2 = foo1 & field @"foo" %~ (maybeToList . fmap show)
```
This example shows that higher-kinded parameters can also be changed (`Maybe` -> `[]`).

### By position

Fields can be accessed by their position in the data structure (index starting at 1):

```haskell
data Point = Point Int Int Int deriving (Generic, Show)
data Polygon = Polygon Point Point Point deriving (Generic, Show)

polygon :: Polygon
polygon = Polygon (Point 1 5 3) (Point 2 4 2) (Point 5 7 (-2))
```

```haskell
>>> polygon ^. position @1 . position @2
5

>>> polygon & position @3 . position @2 %~ (+10)
Polygon (Point 1 5 3) (Point 2 4 2) (Point 5 17 (-2))

>>> polygon ^. position @10
error:
  • The type Polygon does not contain a field at position 10
```

Since tuples are an instance of `Generic`, the positional lens readily works:

```haskell
>>> (("hello", True), 5) ^. position @1 . position @2
True
>>> (("hello", True, "or"), 5, "even", "longer") ^. position @1 . position @2
True
```

### By type

Fields can be accessed by their type in the data structure, assuming that this
type is unique:

```haskell
>>> sally ^. typed @String
"Sally"

>>> setTyped @Int sally 26
Person {name = "Sally", age = 26}
```

### By structure

The `super` lens generalises the `field` lens to focus on a collection rather
than a single field.
We can say that a record is a (structural) `subtype' of another, if its fields
are a superset of those of the other.

```haskell
data Human = Human
  { name    :: String
  , age     :: Int
  , address :: String
  } deriving (Generic, Show)

data Animal = Animal
  { name    :: String
  , age     :: Int
  } deriving (Generic, Show)

human :: Human
human = Human {name = "Tunyasz", age = 50, address = "London"}
```

```haskell
>>> human ^. super @Animal
Animal {name = "Tunyasz", age = 50}

>>> upcast human :: Animal
Animal {name = "Tunyasz", age = 50}
```

We can apply a function that operates on a supertype to the larger (subtype)
structure, by focusing on the supertype first:

```haskell
growUp :: Animal -> Animal
growUp (Animal name age) = Animal name (age + 50)

>>> human & super @Animal %~ growUp
Human {name = "Tunyasz", age = 60, address = "London"}
```

## Traversals

Traversals allow multiple values to be queried or updated at the same time.

### By type
```haskell
types :: HasTypes s a => Traversal s s a a
```
TODO.

### By parameter
TODO.
```haskell
param :: HasParam pos s t a b => Traversal s t a b
```

### By constraint
TODO.
```haskell
constraints  :: HasConstraints c s t => Applicative g => (forall a b . c a b => a -> g b) -> s -> g t
```

## Prisms

A prism focuses on one part of a sum type (which might not be present). Other
than querying the type, they can be "turned around" to inject the data into the
sum (like a constructor).

### By name

Constructor components can be accessed using the constructor's name:

```haskell
type Name = String
type Age  = Int

data Dog = MkDog { name :: Name, age :: Age } deriving (Generic, Show)
data Animal = Dog Dog | Cat Name Age | Duck Age deriving (Generic, Show)

shep = Dog (MkDog "Shep" 4)
mog = Cat "Mog" 5
donald = Duck 4
```

```haskell
>>> shep ^? _Ctor @"Dog"
Just (MkDog {name = "Shep", age = 4})

>>> shep ^? _Ctor @"Cat"
Nothing
```

Constructors with multiple fields can be focused as a tuple

```
>>> mog ^? _Ctor @"Cat"
Just ("Mog",5)

>>> _Ctor @"Cat" # ("Garfield", 6) :: Animal
Cat "Garfield" 6

```

### By type

Constructor components can be accessed using the component's type, assuming
that this type is unique:

```haskell
type Name = String
type Age  = Int

data Dog = MkDog { name :: Name, age :: Age } deriving (Generic, Show)
data Animal = Dog Dog | Cat Name Age | Duck Age deriving (Generic, Show)

shep = Dog (MkDog "Shep" 4)
mog = Cat "Mog" 5
donald = Duck 4
```

```haskell
>>> mog ^? _Typed @Dog
Nothing

>>> shep ^? _Typed @Dog
Just (MkDog {name = "Shep", age = 4})

>>> donald ^? _Typed @Age
Just 4

>>> mog ^? _Typed @(Name, Age)
("Mog", 5)

>>> donald ^? _Typed @Float
error:
  • The type Animal does not contain a constructor whose field is of type Float

>>> _Typed @Age # 6 :: Animal
Duck 6
```

## Contributors

+ [Matthew Pickering](https://github.com/mpickering)
+ [Toby Shaw](https://github.com/TobyShaw)
+ [Will Jones](https://github.com/lunaris)

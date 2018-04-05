# generic-lens

[![Build Status](https://travis-ci.org/kcsongor/generic-lens.svg?branch=master)](https://travis-ci.org/kcsongor/generic-lens)
[![Hackage](https://img.shields.io/hackage/v/generic-lens.svg)](https://hackage.haskell.org/package/generic-lens)

Generically derive traversals, lenses and prisms.

Available on [Hackage](https://hackage.haskell.org/package/generic-lens)

This library uses `GHC.Generics` to derive efficient optics (traversals, lenses
and prisms) for algebraic data types in a type-directed way, with a focus on
good type inference and error messages when possible.

The derived optics use the so-called van Laarhoven representation, thus are
fully interoperable with the combinators found in mainstream lens libraries.

Examples can be found in the `examples` and `tests` folders.

Table of contents
=================

* [Preliminaries](#preliminaries)
* [Taxonomy of optics](#taxonomy-of-optics)
   * [Lenses](#lenses)
      * [By name](#by-name)
      * [By position](#by-position)
      * [By type](#by-type)
      * [By structure](#by-structure)
   * [Traversals](#traversals)
      * [By type](#by-type-1)
      * [By parameter](#by-parameter)
      * [By constraint](#by-constraint)
   * [Prisms](#prisms)
      * [By name](#by-name-1)
      * [By type](#by-type-2)
* [Performance](#performance)
* [Contributors](#contributors)

# Preliminaries
A typical module using `generic-lens` will usually have the following
extensions turned on:
```haskell
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

```

# Taxonomy of optics
Here is a comprehensive list of the optics exposed by `generic-lens`. The
combinators each allow a different way of identifying certain parts of
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
data Two
 = First  { wurble :: String, banana :: Int }
 | Second { wurble :: String }
 deriving (Generic, Show)

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
This example shows that higher-kinded parameters can also be changed (`Maybe`
-> `[]`). We turn a `Foo Maybe Int` into a `Foo [] String` by turning the inner
`Maybe Int` into a `[String]`.

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

As a running example, consider a data type of weighted trees. There are two
type parameters, which correspond to the type of elements and weights in the
tree:
```haskell
data WTree a w
  = Leaf a
  | Fork (WTree a w) (WTree a w)
  | WithWeight (WTree a w) w
  deriving (Generic, Show)

mytree :: WTree Int Int
mytree = Fork (WithWeight (Leaf 42) 1)
              (WithWeight (Fork (Leaf 88) (Leaf 37)) 2)
```

### By type
Focus on all values of a given type.

```haskell
types :: HasTypes s a => Traversal' s a
```

```haskell
>>> toListOf (types @Int) myTree
[42,1,88,37,2]
```

Note that this traversal is `deep': the subtrees are recursively traversed.

### By parameter
As the above example shows, the `types` traversal is limited in that it cannot
distinguish between the two types of `Int`s: the weights and the values.

Instead, the `param` traversal allows specifying types that correspond to a
certain type parameter.
```haskell
param :: HasParam pos s t a b => Traversal s t a b
```

```haskell
>>> toListOf (param @1) myTree
[42,88,37]
```

Here, the numbering starts from 0, with 0 being the rightmost parameter.
Because `param` is guaranteed to focus on parametric values, it allows the type
to be changed as well.

For example, we can implement `Functor` for `WTree` as simply as:

```haskell
instance Functor (WTree a) where
  fmap = over (param @0)
```

### By constraint
The most general type of traversal: we can apply a given function to every
value in a structure, by requiring that all values have an instance for some
type class.

```haskell
constraints   :: HasConstraints c s t => Applicative g => (forall a b . c a b => a -> g b) -> s -> g t
constraints'  :: HasConstraints' c s  => Applicative g => (forall a . c a => a -> g b) -> s -> g s
```

Consider the `Numbers` type, which contains three different numeric types:
```haskell
data Numbers = Numbers Int Float Double
  deriving (Show, Generic)

numbers = Numbers 10 20.0 30.0
```

With `constraints'`, we can uniformly add 20 to each number in one go:
```haskell
>>> constraints' @Num (\x -> pure (x + 20)) numbers
Numbers 30 40.0 50.0
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

# Performance
TODO.
# Contributors

+ [Matthew Pickering](https://github.com/mpickering)
+ [Toby Shaw](https://github.com/TobyShaw)
+ [Will Jones](https://github.com/lunaris)

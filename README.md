# generic-lens

[![Build
Status](https://travis-ci.org/kcsongor/generic-lens.svg?branch=master)](https://travis-ci.org/kcsongor/generic-lens)

Generically derive lenses and prisms for data types.

Available on [Hackage](https://hackage.haskell.org/package/generic-lens)

This package uses the GHC 8 `Generic` representation to derive various operations
on data structures with lens interfaces, including structural subtype
relationships between records and positional indexing into arbitrary product
types.

This is made possible by GHC 8's new Generics API, which provides metadata
at the type-level (previously only value-level metadata was available).

Examples can be found in the `examples` folder. This library makes heavy use of
[Visible Type Applications](https://ghc.haskell.org/trac/ghc/wiki/TypeApplication).

## Lenses

### Record fields

Record fields can be accessed by their label:

```haskell
data Person = Person { name :: String, age :: Int } deriving (Generic, Show)

sally :: Person
sally = Person "Sally" 25
```

```haskell
>>> getField @"age" sally
25

>>> setField @"age" 26 sally
Person {name = "Sally", age = 26}

>>> sally ^. field @"name"
"Sally"

>>> sally & field @"name" .~ "Tamas"
Person {name = "Tamas", age = 25}

>>> sally ^. field @"pet"
error:
  • The type Person does not contain a field named "pet"
```

### Positional fields

Fields can be accessed by their position in the data structure (index starting at 1):

```haskell
data Point = Point Int Int Int deriving (Generic, Show)
data Polygon = Polygon Point Point Point deriving (Generic, Show)

polygon :: Polygon
polygon = Polygon (Point 1 5 3) (Point 2 4 2) (Point 5 7 (-2))
```

```haskell
>>> getPosition @2 polygon
Point 2 4 2

>>> setPosition @1 (Point 26 5 3) polygon
Polygon (Point 26 5 3) (Point 2 4 2) (Point 5 7 (-2))

>>> polygon ^. position @1 . position @2
5

>>> polygon & position @3 . position @2 %~ (+10)
Polygon (Point 1 5 3) (Point 2 4 2) (Point 5 17 (-2))

>>> polygon ^. position @10
error:
  • The type Polygon does not contain a field at position 10
```

Since tuples are an instance of `Generic`, they also have positional lenses:

```haskell
>>> (("hello", True), 5) ^. position @1 . position @2
True
```

### Typed fields

Fields can be accessed by their type in the data structure, assuming that this
type is unique:

```haskell
data Person = Person { name :: String, age :: Int } deriving (Generic, Show)
data Point = Point Int Int Int deriving (Generic, Show)

sally :: Person
sally = Person "Sally" 25

point :: Point
point = Point 1 2 3
```

```haskell
>>> getTyped @String sally
"Sally"

>>> setTyped @Int sally 26
Person {name = "Sally", age = 26}

>>> point ^. typed @Int
error:
  • The type Point contains multiple values of type Int; the choice of value is thus ambiguous

>>> point & typed @String .~ "Point"
error:
  • The type Point does not contain a value of type [Char]
```

### Structural subtyping

A record is a (structural) `subtype' of another, if its fields are a superset of
those of the other.

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

>>> upcast human :: Animal
Animal {name = "Tunyasz", age = 50}

-- 'smash' plug the smaller structure into the larger one
>>> smash (Animal "dog" 10) human
Human {name = "dog", age = 10, address = "London"}

-- 'super' is a lens that focuses on a subrecord of a larger record:
>>> human ^. super @Animal
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

## Prisms

### Named constructors

Constructor components can be accessed using the constructor's name:

```haskell
type Name = String
type Age  = Int

data Dog = MkDog { name :: Name, age :: Age } deriving (Generic, Show)
data Animal = Dog Dog | Cat (Name, Age) | Duck Age deriving (Generic, Show)

shep = Dog (MkDog "Shep" 4)
mog = Cat ("Mog", 5)
donald = Duck 4
```

```haskell
>>> shep ^? _Ctor @"Dog"
Just (MkDog {name = "Shep", age = 4})

>>> shep ^? _Ctor @"Cat"
Nothing

>>> mog ^? _Ctor @"Cat"
Just ("Mog",5)

>>> _Ctor @"Cat" # ("Garfield", 6) :: Animal
Cat ("Garfield",6)

>>> donald ^? _Ctor @"Giraffe"
error:
  • The type Animal does not contain a constructor named "Giraffe"
```

### Typed constructors

Constructor components can be accessed using the component's type, assuming
that this type is unique:

```haskell
type Name = String
type Age  = Int

data Dog = MkDog { name :: Name, age :: Age } deriving (Generic, Show)
data Animal = Dog Dog | Cat (Name, Age) | Duck Age deriving (Generic, Show)

shep = Dog (MkDog "Shep" 4)
mog = Cat ("Mog", 5)
donald = Duck 4
```

```haskell
>>> mog ^? _Typed @Dog
Nothing

>>> shep ^? _Typed @Dog
Just (MkDog {name = "Shep", age = 4})

>>> donald ^? _Typed @Age
Just 4

>>> donald ^? _Typed @Float
error:
  • The type Animal does not contain a constructor whose field is of type Float

>>> _Typed @Age # 6 :: Animal
Duck 6
```

## Contributors

+ [Toby Shaw](https://github.com/TobyShaw)
+ [Will Jones](https://github.com/lunaris)

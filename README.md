# generic-lens
Generically derive lenses for accessing fields of data types.

Available on [Hackage](https://hackage.haskell.org/package/generic-lens)

This package uses the GHC 8 Generic representation to derive various operations on data structures with a lens interface, including structural subtype relationship between records and positional indexing into arbitrary product types.

This is made possible by GHC-8's new Generics API, which provides the meta-data
at the type-level (previously only value-level meta-data was available).

An example can be found in the `examples` folder.

(The library makes heavy use of [Visible Type Applications](https://ghc.haskell.org/trac/ghc/wiki/TypeApplication))

## Record fields

Record fields can be accessed by their label.

```haskell
data Person = Person { name :: String, age :: Int } deriving (Show, Generic)

sally :: Person
sally = Person "Sally" 25
```

```haskell
>>> getField @"age" sally
25

>>> setField @"age" 26 sally
Person { name = "Sally", age = 26 }

>>> sally ^. label @"name"
"Sally"

>>> sally & label @"name" .~ "Tamas"
Person { name = "Tamas", age = 25 }

```

## Positional fields

Fields can be accessed by their position in the data structure (index starting at 1).

```haskell
data Point = Point Int Int Int deriving (Show, Generic)
data Polygon = Polygon Point Point Point deriving (Show, Generic)

polygon :: Polygon
polygon = Polygon (Point 1 5 3) (Point 2 4 2) (Point 5 7 (-2))
```

```haskell

>>> getFieldAt @2 polygon
Point 2 4 2

>>> setFieldAt @1 (Point 26 5 3) polygon
Polygon (Point 26 5 3) (Point 2 4 2) (Point 5 7 (-2))

>>> polygon ^. itemAt @1 . itemAt @2
5

>>> polygon & itemAt @3 . itemAt @2 %~ (+10)
Polygon (Point 1 5 3) (Point 2 4 2) (Point 5 17 (-2))

```

Since tuples are an instance of Generic, positional lenses can be used with them.

```haskell
>>> (("hello", True), 5) ^. itemAt @1 . itemAt @2
True
```

## Structural subtyping

A record is a (structural) `subtype' of another, if its fields are a subset of
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
human = Human { name = "Tunyasz", age = 50, address = "London" }
```

```haskell

>>> upcast human :: Animal
Animal {name = "Tunyasz", age = 50}

-- 'smash' plug the smaller structure into the larger one
>>> smash (Animal "dog" 10) human
Human {name = "dog", age = 10, address = "London"}

-- 'subtype' is a lens that focuses on a subrecord of a larger record:
>>> human ^. subtype @Animal
Animal {name = "Tunyasz", age = 50}
```

We can apply a function that operates on a subtype to the larger structure,
by focusing on the subtype first:

```haskell
growUp :: Animal -> Animal
growUp (Animal name age) = Animal name (age + 50)

>>> human & subtype @Animal %~ growUp
Human {name = "Tunyasz", age = 60, address = "London"}
```

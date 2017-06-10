# generic-lens
Magic record operations using generics (i.e. both type- and morally safe - no TH)

Available on [Hackage](https://hackage.haskell.org/package/generic-lens)

This package uses the GHC8 Generic representation of records to derive magic classes (similar to the OverloadedRecordLabels proposal), and prove structural subtyping relationship between records.
The latter can be used to upcast the "sub-record" to the more general "interface".

This is made possible by GHC-8's new Generics API, which provides the meta-data
at the type-level (previously only value-level meta-data was available).

An example can be found in the `examples` folder.

## Magic getters

```haskell
getName :: HasField r "name" a => r -> a
getName r = getField r (Proxy @"name")
```
defines a function that accepts any record that has a `name` field of type `a`,
then simply returns the field.

## Structural subtyping

```haskell
data R1 = R1 { a :: String, b :: Int }
data R2 = R2 { a :: String }

upcast (R1 "hello" 5) :: R2
-- => R2 "hello"
```

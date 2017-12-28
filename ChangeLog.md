## 0.6.0.0
- Infer input type from result type (#25)
- Allow changing of multiple type parameters (#24)
- Allow changing of type parameters that have kinds other than `*` (#23)

## 0.5.1.0
- Add `IsLabel` orphan instance in `Data.Generics.Labels`
- Fix error message in subtype lens

## 0.5.0.0

- Lenses and prisms are now type-changing.
- More informative error messages
- More readable type signatures in type errors and when using `:t`
- Use `doctest`
- Include examples in Haddock

### Breaking API changes

- The type parameters of the classes have been changed to accommodate
  the type-changing update:
  
  `class HasField name a s` -> `class HasField name s t a b` etc.
  
  Accordingly, `field :: Lens' s a` -> `field :: Lens s t a b`

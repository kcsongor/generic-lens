## 1.2.0.0
- Add `HasTypesUsing` and `HasTypesCustom` for custom traversals (Lysxia)

## 1.1.0.0
- Fix regression in type inference for polymorphic optics
- Add `HasField0`, `HasPosition0`, `AsConstructor0`, `HasField_`, `HasPositon_`, and `AsConstructor_` (Lysxia)
- `types` now supports Data.Word and Data.Int (Lysxia)
- Add `Wrapped` iso for newtypes (Isaac Elliott)
- Expose internals through Data.GenericLens.Internal
- Add labels for prisms (Daniel Winograd-Cort)

## 1.0.0.2
- Fix compile-time performance regression

## 1.0.0.1
- Remove dump-core dependency
- Relax upper bound on criterion (#42)

## 1.0.0.0
- Traversals (types, param, constraints)
- Prisms are now optimal too
- Monomorphic versions of lenses and prisms also included

### Breaking API changes
- `projectSub` now returns `Maybe sub` instead of `Either sup sub` (#21)

## 0.5.1.0
- Infer input type from result type (#25)
- Allow changing of multiple type parameters (#24)
- Allow changing of type parameters that have kinds other than `*` (#23)
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

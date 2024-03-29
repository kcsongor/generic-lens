## Unreleased
- Add `OverloadedLabels` support for positional lenses, e.g. `#3` as an
  abbreviation for `position @3`, starting with GHC 9.6.

## generic-lens-2.2.2.0 (2023-04-15)
- Support unprefixed constructor prisms on GHC 9.6 (#152)

## generic-lens-2.2.1.0 (2022-01-22)
- GHC 9.2 compatibility

## generic-lens-2.2.0.0 (2021-07-13)
- GHC 9.0 compatibility

## generic-lens-2.1.0.0 (2021-01-25)
- Bump to generic-lens-core-2.1.0.0

## generic-lens-2.0.0.0 (2020-02-11)
- Drop support for GHC < 8.4
- Better inference for `field'`
- Param traversal now properly recurses deeply (#88)
- Reorganise internals (see generic-lens-core)

### Breaking API changes:
- `HasTypesUsing` now takes 4 params
- Removed `HasConstraints` traversal

## generic-lens-1.2.0.1
- Give HasAny/AsAny the same VTA behavior on 8.6 and 8.8 (Ryan Scott)

## generic-lens-1.2.0.0
- Add `HasTypesUsing` and `HasTypesCustom` for custom traversals (Lysxia)
- Improve type errors when no Generic instance is defined
- `types` now supports Text by default

### Breaking API changes
- `HasType` now includes a reflexive case so that every type 'contains' itself (Matt Parsons)
- `AsSubtype` and `Subtype` now include a reflexive case so that every type is a subtype of itself

## generic-lens-1.1.0.0
- Fix regression in type inference for polymorphic optics
- Add `HasField0`, `HasPosition0`, `AsConstructor0`, `HasField_`, `HasPositon_`, and `AsConstructor_` (Lysxia)
- `types` now supports Data.Word and Data.Int (Lysxia)
- Add `Wrapped` iso for newtypes (Isaac Elliott)
- Expose internals through Data.GenericLens.Internal
- Add labels for prisms (Daniel Winograd-Cort)

## generic-lens-1.0.0.2
- Fix compile-time performance regression

## generic-lens-1.0.0.1
- Remove dump-core dependency
- Relax upper bound on criterion (#42)

## generic-lens-1.0.0.0
- Traversals (types, param, constraints)
- Prisms are now optimal too
- Monomorphic versions of lenses and prisms also included

### Breaking API changes
- `projectSub` now returns `Maybe sub` instead of `Either sup sub` (#21)

## generic-lens-0.5.1.0
- Infer input type from result type (#25)
- Allow changing of multiple type parameters (#24)
- Allow changing of type parameters that have kinds other than `*` (#23)
- Fix error message in subtype lens

## generic-lens-0.5.0.0

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

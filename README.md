# generic-lens

[![Build Status](https://travis-ci.org/kcsongor/generic-lens.svg?branch=master)](https://travis-ci.org/kcsongor/generic-lens)
[![Hackage](https://img.shields.io/hackage/v/generic-lens.svg)](https://hackage.haskell.org/package/generic-lens)

Generically derive isos, traversals, lenses and prisms.

This library uses `GHC.Generics` to derive efficient optics (traversals, lenses
and prisms) for algebraic data types in a type-directed way, with a focus on
good type inference and error messages when possible.

The library is described in the paper:
> Csongor Kiss, Matthew Pickering, and Nicolas Wu. 2018. Generic deriving of generic traversals. Proc. ACM Program. Lang. 2, ICFP, Article 85 (July 2018), 30 pages. DOI: https://doi.org/10.1145/3236780

## Package structure

* [`generic-lens`](https://hackage.haskell.org/package/generic-lens):
  Generic derivation of van Laarhoven optics, compatible with the
  [`lens`](https://hackage.haskell.org/package/lens) library. There
  is no dependency on `lens`, however, and other mainstream lens libraries
  sharing the same interface are supported just as well.
  
* [`generic-optics`](https://hackage.haskell.org/package/generic-optics):
  Generic derivation of optics compatible with the [`optics`](https://hackage.haskell.org/package/optics)
  library.

* [`generic-lens-core`](https://hackage.haskell.org/package/generic-lens-core):
  The bulk of the work happens here. Indeed, `generic-lens` and
  `generic-optics` are just thin layers on top of `generic-lens-core`.
  
`generic-lens` are `generic-optics` are designed to be drop-in
replacements for each other. This means that they share the same module names. If, for
whatever reason, they are both used in the same project, the module imports can be disambiguated
using the `-XPackageImports` extension.

## Getting started
A typical module using `generic-lens` or `generic-optics` will usually have the following
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

In particular, `NoMonomorphismRestriction` can be helpful as it
enables proper type inference for top-level functions without a type
signature.

For usage examples, visit the documentation on hackage, or have a look
in the `examples` and `tests` folders in `generic-lens` or
`generic-optics`.

## Performance
The runtime characteristics of the derived optics is in most cases identical at
`-O1`, in some cases only slightly slower than the hand-written version. This
is thanks to GHC's optimiser eliminating the generic overhead.

The
[inspection-testing](https://hackage.haskell.org/package/inspection-testing)
library is used to ensure (see [here](test/Spec.hs)) that everything gets
inlined away.

There is also a [benchmark suite](https://github.com/mpickering/generic-lens-benchmarks) with larger, real-world examples.

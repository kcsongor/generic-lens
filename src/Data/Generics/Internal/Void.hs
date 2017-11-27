module Data.Generics.Internal.Void where

{-
  Note [Uncluttering type signatures]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Because the various instances in the library always match (the Has* classes
  are essentially glorified constraint synonyms), they get replaced with
  their constraints, resulting in large, unreadable types.

  Writing an (overlapping instance) for this Void type means that the original
  instance might not be the one selected, thus GHC leaves the constraints in
  place until further information is provided, at which point the type
  machinery has sufficient information to reduce to sensible types.
-}
data Void

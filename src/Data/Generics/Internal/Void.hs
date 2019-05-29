{-# LANGUAGE PolyKinds, EmptyCase #-}
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
data Void1 a
data Void2 a b

absurd :: Void -> x
absurd v = case v of

absurd1 :: Void1 a -> x
absurd1 v = case v of

absurd2 :: Void2 a b -> x
absurd2 v = case v of

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Data.Generics.Internal.Wrapped
  ( Context
  , derived
  ) where

import Data.Generics.Internal.Profunctor.Iso

import Data.Generics.Internal.Families.Changing ( UnifyHead )

import Data.Kind (Constraint)
import GHC.Generics
import GHC.TypeLits

type Context s t a b
  = ( Generic s
    , Generic t
    , GWrapped (Rep s) (Rep t) a b
    , UnifyHead s t
    , UnifyHead t s
    , ErrorUnlessOnlyOne s (Rep s)
    )

derived :: Context s t a b => Iso s t a b
derived = repIso . gWrapped
{-# INLINE derived #-}

type family ErrorUnlessOnlyOne a b :: Constraint where
  ErrorUnlessOnlyOne t (M1 i k a) = ErrorUnlessOnlyOne t a
  ErrorUnlessOnlyOne t (K1 i a) = ()
  ErrorUnlessOnlyOne t a =
    TypeError ('ShowType t ':<>: 'Text " is not a single-constructor, single-field datatype")

--------------------------------------------------------------------------------

class GWrapped s t a b | s -> a, t -> b, s b -> t, t a -> s where
  gWrapped :: Iso (s x) (t x) a b

instance GWrapped s t a b => GWrapped (M1 i k s) (M1 i k t) a b where
  gWrapped = mIso . gWrapped
  {-# INLINE gWrapped #-}

instance (a ~ c, b ~ d) => GWrapped (K1 i a) (K1 i b) c d where
  gWrapped = kIso
  {-# INLINE gWrapped #-}

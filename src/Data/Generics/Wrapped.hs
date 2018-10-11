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

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Wrapped
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a newtype and its wrapped type.
--
-----------------------------------------------------------------------------

module Data.Generics.Wrapped
  ( Wrapped(..)
  , _Unwrapped
  , _Wrapped
  )
where

import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.VL.Lens ((^.))

import qualified Data.Generics.Internal.VL.Iso as VL

import Data.Kind (Constraint)
import GHC.Generics
import GHC.TypeLits

class GWrapped a b | a -> b where
  gWrapped :: Iso' (a x) b

instance GWrapped a b => GWrapped (M1 i k a) b where
  gWrapped = mIso.gWrapped

instance GWrapped (K1 i c) c where
  gWrapped = kIso

-- | @since 1.1.0.0
class Wrapped a b | a -> b where
  {-# minimal wrappedIso | wrappedTo, wrappedFrom #-}
  -- | @since 1.1.0.0
  wrappedIso :: VL.Iso' a b
  wrappedIso = VL.iso wrappedTo wrappedFrom
  {-# INLINE wrappedIso #-}

  -- | @since 1.1.0.0
  wrappedTo :: a -> b
  wrappedTo a = a ^. wrappedIso
  {-# INLINE wrappedTo #-}

  -- | @since 1.1.0.0
  wrappedFrom :: b -> a
  wrappedFrom a = a ^. VL.fromIso wrappedIso
  {-# INLINE wrappedFrom #-}

type family ErrorUnlessOnlyOne a b :: Constraint where
  ErrorUnlessOnlyOne t (M1 i k a) = ErrorUnlessOnlyOne t a
  ErrorUnlessOnlyOne t (K1 i a) = ()
  ErrorUnlessOnlyOne t a =
    TypeError ('ShowType t ':<>: 'Text " is not a single-constructor, single-field datatype")

instance (Generic a, ErrorUnlessOnlyOne a (Rep a), GWrapped (Rep a) b) => Wrapped a b where
  wrappedIso = iso2isovl (repIso . gWrapped)
  {-# INLINE wrappedIso #-}

-- | @since 1.1.0.0
_Unwrapped :: Wrapped a b => VL.Iso' a b
_Unwrapped = wrappedIso
{-# INLINE _Unwrapped #-}

-- | @since 1.1.0.0
_Wrapped :: Wrapped a b => VL.Iso' b a
_Wrapped = VL.fromIso wrappedIso
{-# INLINE _Wrapped #-}

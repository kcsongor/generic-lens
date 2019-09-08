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

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Wrapped
-- Copyright   :  (C) 2019 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a newtype and its wrapped type.
--
-----------------------------------------------------------------------------

module Data.Generics.Wrapped
  ( Wrapped (..)
  , wrappedTo
  , wrappedFrom
  , _Unwrapped
  , _Wrapped
  )
where

import Data.Generics.Internal.Profunctor.Iso

import Data.Generics.Internal.Families.Changing ( UnifyHead )

import Data.Kind (Constraint)
import GHC.Generics
import GHC.TypeLits

type family ErrorUnlessOnlyOne a b :: Constraint where
  ErrorUnlessOnlyOne t (M1 i k a) = ErrorUnlessOnlyOne t a
  ErrorUnlessOnlyOne t (K1 i a) = ()
  ErrorUnlessOnlyOne t a =
    TypeError ('ShowType t ':<>: 'Text " is not a single-constructor, single-field datatype")

-- | @since 1.1.0.0
_Unwrapped :: Wrapped s t a b => Iso s t a b
_Unwrapped = wrappedIso
{-# inline _Unwrapped #-}

-- | @since 1.1.0.0
_Wrapped :: Wrapped s t a b => Iso b a t s
_Wrapped = fromIso wrappedIso
{-# inline _Wrapped #-}

-- TODO: move this into doctets

-- newtype FlippedEither a b = FlippedEither (Either b a)
--   deriving Generic

-- test :: (a -> c) -> FlippedEither a b -> FlippedEither c b
-- test f = over wrappedIso (fmap f)

class GWrapped s t a b | s -> a, t -> b, s b -> t, t a -> s where
  gWrapped :: Iso (s x) (t x) a b

instance GWrapped s t a b => GWrapped (M1 i k s) (M1 i k t) a b where
  gWrapped = mIso . gWrapped

instance (a ~ c, b ~ d) => GWrapped (K1 i a) (K1 i b) c d where
  gWrapped = kIso

-- | @since 1.1.0.0
class Wrapped s t a b | s -> a, t -> b where
  -- | @since 1.1.0.0
  wrappedIso :: Iso s t a b

-- | @since 1.1.0.0
wrappedTo :: forall s t a b. Wrapped s t a b => s -> a
wrappedTo a = view (wrappedIso @s @t @a @b) a
{-# INLINE wrappedTo #-}

-- | @since 1.1.0.0
wrappedFrom :: forall s t a b. Wrapped s t a b => b -> t
wrappedFrom a = view (fromIso (wrappedIso @s @t @a @b)) a
{-# INLINE wrappedFrom #-}
  
instance
  ( Generic s
  , Generic t
  , GWrapped (Rep s) (Rep t) a b
  , UnifyHead s t
  , UnifyHead t s
  ) => Wrapped s t a b where
  wrappedIso = {-iso2isovl-} (repIso . gWrapped)
  {-# INLINE wrappedIso #-}

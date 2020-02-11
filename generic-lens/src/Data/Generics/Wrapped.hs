{-# LANGUAGE PackageImports #-}
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
-- Copyright   :  (C) 2020 Csongor Kiss
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

import qualified "this" Data.Generics.Internal.VL.Iso as VL

import "generic-lens-core" Data.Generics.Internal.Wrapped (Context, derived)

import Control.Applicative    (Const(..))


-- | @since 1.1.0.0
_Unwrapped :: Wrapped s t a b => VL.Iso s t a b
_Unwrapped = wrappedIso
{-# inline _Unwrapped #-}

-- | @since 1.1.0.0
_Wrapped :: Wrapped s t a b => VL.Iso b a t s
_Wrapped = VL.fromIso wrappedIso
{-# inline _Wrapped #-}

-- | @since 1.1.0.0
class Wrapped s t a b | s -> a, t -> b where
  -- | @since 1.1.0.0
  wrappedIso :: VL.Iso s t a b

-- | @since 1.1.0.0
wrappedTo :: forall s t a b. Wrapped s t a b => s -> a
wrappedTo a = view (wrappedIso @s @t @a @b) a
  where view l s = getConst (l Const s)
{-# INLINE wrappedTo #-}

-- | @since 1.1.0.0
wrappedFrom :: forall s t a b. Wrapped s t a b => b -> t
wrappedFrom a = view (VL.fromIso (wrappedIso @s @t @a @b)) a
  where view l s = getConst (l Const s)
{-# INLINE wrappedFrom #-}

instance Context s t a b => Wrapped s t a b where
  wrappedIso = VL.iso2isovl derived
  {-# INLINE wrappedIso #-}

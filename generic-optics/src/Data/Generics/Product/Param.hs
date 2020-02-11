{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Product.Param
-- Copyright   : (C) 2020 Csongor Kiss
-- License     : BSD3
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Derive traversals over type parameters
--
--------------------------------------------------------------------------------

module Data.Generics.Product.Param
  ( Rec (Rec) -- TODO: this has to be re-exported so the constructor is visible for Coercible... is there a better way?
  , HasParam (..)
  , Param (..)
  ) where

import Data.Generics.Internal.Optics
import "this" Data.Generics.Product.Types

import qualified "generic-lens-core" Data.Generics.Internal.VL.Traversal as VL
import qualified "generic-lens-core" Data.Generics.Product.Internal.Param as Core
import "generic-lens-core" Data.Generics.Internal.GenericN
import "generic-lens-core" Data.Generics.Internal.Void

import GHC.TypeLits

class HasParam (p :: Nat) s t a b | p t a -> s, p s b -> t, p s -> a, p t -> b where
  param :: Traversal s t a b

instance
  ( Core.Context n s t a b
  , GHasTypes ChGeneric (RepN s) (RepN t) (Param n a) (Param n b)
  ) => HasParam n s t a b where

  param = traversalVL (VL.confusing (repIsoN . (gtypes_ @ChGeneric) . paramIso @n))
  {-# INLINE param #-}

-- this could be an iso but since we're operating on a VL traversal it's easier this way.
repIsoN :: (GenericN a, GenericN b) => VL.Traversal a b (RepN a x) (RepN b x)
repIsoN f a = toN <$> f (fromN a)

-- this could be an iso but since we're operating on a VL traversal it's easier this way.
paramIso :: VL.Traversal (Param n a) (Param n b) a b
paramIso f a = StarParam <$> f (getStarParam a)

instance {-# OVERLAPPING #-} HasParam p (Void1 a) (Void1 b) a b where
  param = undefined

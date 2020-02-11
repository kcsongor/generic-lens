{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.HList
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a product type and a flat HList.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.HList
  ( IsList (..)
  ) where

import "this" Data.Generics.Internal.Optics

import "generic-lens-core" Data.Generics.Internal.Profunctor.Iso (repIso)
import qualified "generic-lens-core" Data.Generics.Product.Internal.HList as Core

import Data.Kind
import GHC.Generics

class IsList
  (f :: Type)
  (g :: Type)
  (as :: [Type])
  (bs :: [Type]) | f -> as, g -> bs where
  list  :: Iso f g (Core.HList as) (Core.HList bs)

instance
  ( Generic f
  , Generic g
  , Core.GIsList (Rep f) (Rep g) as bs
  ) => IsList f g as bs where
  list = normaliseIso (Optic (repIso . Core.glist))
  {-# INLINE list #-}

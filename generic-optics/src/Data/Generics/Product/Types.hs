{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Types
-- Copyright   :  (C) 2019 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Types
  ( -- *Traversals
    --
    -- $setup
    L.HasTypes
  , types

  ) where

-- TODO: support custom traversal strategies
import qualified Data.Generics.Product.Internal.Types as L
import Optics.Core

types :: forall a s. L.HasTypes s a => Traversal' s a
types = traversalVL (L.types_ @s @a)
{-# INLINE types #-}

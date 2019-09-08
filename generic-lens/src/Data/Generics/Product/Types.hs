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
    HasTypes
  , types

    -- * Custom traversal strategies
    -- $custom
  , Children
  , ChGeneric

  , HasTypesUsing
  , typesUsing

  , HasTypesCustom (typesCustom)

  ) where


import Data.Generics.Product.Internal.Types

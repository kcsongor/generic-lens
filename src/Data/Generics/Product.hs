-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Magic product operations using Generics
--
-- These classes need not be instantiated manually, as GHC can automatically
-- prove valid instances via Generics. Only the `Generic` class needs to
-- be derived (see examples).
--
-----------------------------------------------------------------------------

module Data.Generics.Product
  ( -- *Lenses
    module Data.Generics.Product.Any
  , module Data.Generics.Product.Fields
  , module Data.Generics.Product.Positions
  , module Data.Generics.Product.Subtype
  , module Data.Generics.Product.Typed
  , module Data.Generics.Product.Types
  , module Data.Generics.Product.List
  ) where

import Data.Generics.Product.Any
import Data.Generics.Product.Fields
import Data.Generics.Product.Positions
import Data.Generics.Product.Subtype
import Data.Generics.Product.Typed
import Data.Generics.Product.Types
import Data.Generics.Product.List

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
    module Any
  , module Field
  , module Positions
  , module Subtype
  , module Typed
  ) where

import Data.Generics.Product.Any       as Any
import Data.Generics.Product.Fields    as Field
import Data.Generics.Product.Positions as Positions
import Data.Generics.Product.Subtype   as Subtype
import Data.Generics.Product.Typed     as Typed

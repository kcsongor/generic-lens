-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Record
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
  (
    -- * Magic lens
    HasFieldAt  (..)

    -- * Getter and setter
  , getFieldAt
  , setFieldAt

  ) where

import Data.Generics.Product.HasFieldAt

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Magic sum operations using Generics
--
-- These classes need not be instantiated manually, as GHC can automatically
-- prove valid instances via Generics. Only the `Generic` class needs to
-- be derived (see examples).
--
-----------------------------------------------------------------------------

module Data.Generics.Sum
  ( -- *Prisms
    AsAny (..)

  , AsConstructor (..)
  , AsType (..)
  ) where

import Data.Generics.Sum.Any
import Data.Generics.Sum.Constructors
import Data.Generics.Sum.Typed

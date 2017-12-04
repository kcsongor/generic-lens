-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum
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
    module Any
  , module Constructors
  , module Subtype
  , module Typed
  ) where

import Data.Generics.Sum.Any          as Any
import Data.Generics.Sum.Constructors as Constructors
import Data.Generics.Sum.Subtype      as Subtype
import Data.Generics.Sum.Typed        as Typed

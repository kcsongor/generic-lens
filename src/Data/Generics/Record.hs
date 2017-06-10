-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Record
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Magic record operations using Generics
--
-- These classes need not be instantiated manually, as GHC can automatically
-- prove valid instances via Generics. Only the `Generic` class needs to
-- be derived (see examples).
--
-----------------------------------------------------------------------------
module Data.Generics.Record
  (
    -- * Subtype relationship
    Subtype  (..)

    -- * Magic lens
  , HasField (..)

    -- * Getter and setter
  , getField
  , setField

  ) where

import Data.Generics.Record.HasField
import Data.Generics.Record.Subtype

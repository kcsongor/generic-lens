{-# LANGUAGE PackageImports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum
-- Copyright   :  (C) 2020 Csongor Kiss
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
    module Data.Generics.Sum.Any
  , module Data.Generics.Sum.Constructors
  , module Data.Generics.Sum.Subtype
  , module Data.Generics.Sum.Typed
  ) where

import "this" Data.Generics.Sum.Any
import "this" Data.Generics.Sum.Constructors
import "this" Data.Generics.Sum.Subtype
import "this" Data.Generics.Sum.Typed

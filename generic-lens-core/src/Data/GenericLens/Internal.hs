-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GenericLens.Internal
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The library internals are exposed through this module. Please keep
-- in mind that everything here is subject to change irrespective of
-- the the version numbers.
-----------------------------------------------------------------------------

module Data.GenericLens.Internal
  ( module Data.Generics.Internal.Families
  , module Data.Generics.Internal.Families.Changing
  , module Data.Generics.Internal.Families.Collect
  , module Data.Generics.Internal.Families.Has
  , module Data.Generics.Internal.Void
  , module Data.Generics.Internal.Errors

  , module Data.Generics.Internal.GenericN

  -- * Profunctor optics
  , module Data.Generics.Internal.Profunctor.Iso
  , module Data.Generics.Internal.Profunctor.Lens
  , module Data.Generics.Internal.Profunctor.Prism

  , module Data.Generics.Product.Internal.Subtype
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Families.Changing
import Data.Generics.Internal.Families.Collect
import Data.Generics.Internal.Families.Has
import Data.Generics.Internal.Void
import Data.Generics.Internal.Errors

import Data.Generics.Internal.GenericN

import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Lens
import Data.Generics.Internal.Profunctor.Prism

import Data.Generics.Product.Internal.Subtype

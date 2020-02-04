{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Tuple
-- Copyright   :  (C) 2020 Yuriy Syrovetskiy
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a product type and a flat tuple.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Tuple (tuple) where

import Data.GenericLens.Internal (tupled, ListTuple)
import Data.Generics.Internal.Profunctor.Iso (iso2isovl)
import Data.Generics.Product.HList (IsList (list))
import GHC.Generics (Generic, Rep)

import qualified Data.Generics.Internal.VL.Iso as VL

tuple :: (IsList f f as as, ListTuple t as) => VL.Iso' f t
tuple = list . iso2isovl tupled

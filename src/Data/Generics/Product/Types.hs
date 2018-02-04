{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Types
-- Copyright   :  (C) 2017 Csongor Kiss
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
    --  $example
    HasTypes (..)
  ) where

import Data.Generics.Product.Internal.Types

import GHC.Generics (Generic (Rep))
import Data.Generics.Internal.VL.Traversal
import Data.Generics.Internal.VL.Iso

class HasTypes a s where
  types :: Traversal' s a

instance
  ( Generic s
  , GHasTypes (Rep s) '[a]
  ) => HasTypes a s where
  types = confusing (\f -> (repIso . gtypes) (f :> HNil))
  {-# INLINE types #-}

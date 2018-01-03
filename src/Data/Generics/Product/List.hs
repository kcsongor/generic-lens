{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.List
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a product type and a flat HList.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.List
  ( IsList (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Product.Internal.List
import Data.Kind
import GHC.Generics
import GHC.TypeLits

class IsList
  (f :: Type)
  (g :: Type)
  (as :: [(Symbol, Type)])
  (bs :: [(Symbol, Type)]) | f -> as, g -> bs where
  list  :: Iso f g (List as) (List bs)

instance
  ( Generic f
  , Generic g
  , GIsList (Rep f) (Rep g) as bs
  ) => IsList f g as bs where
  list = repIso . glist

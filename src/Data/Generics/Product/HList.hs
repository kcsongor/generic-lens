{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.HList
-- Copyright   :  (C) 2019 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a product type and a flat HList.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.HList
  ( IsList (..)
  ) where

import Data.Generics.Product.Internal.HList
import Data.Kind
import GHC.Generics
-- import Data.Generics.Internal.VL.Iso (Iso)
import Data.Generics.Internal.Profunctor.Iso (repIso, Iso)

class IsList
  (f :: Type)
  (g :: Type)
  (as :: [Type])
  (bs :: [Type]) | f -> as, g -> bs where
  list  :: Iso f g (HList as) (HList bs)

instance
  ( Generic f
  , Generic g
  , GIsList (Rep f) (Rep g) as bs
  ) => IsList f g as bs where
  list = {-iso2isovl-} (repIso . glist)
  {-# INLINE list #-}

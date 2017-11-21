{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
   , types2
  ) where

--import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens
import Data.Generics.Product.Internal.Types

--import Data.Kind    (Constraint, Type)
import GHC.Generics (Generic (Rep))
--import GHC.TypeLits (TypeError, ErrorMessage (..))

class HasTypes a s where
  types :: Traversal' s a

instance
  ( Generic s
  , GHasTypes (Rep s) a
  ) => HasTypes a s where

  types = (repIso . gtypes')
  {-# INLINE types #-}

types2 :: (HasTypes a s) => Traversal' s a
types2 = ravel types

gtypes' :: (GHasTypes f a) => Traversal' (f a) a
gtypes' f s = flatten @(S (S (S (S (S (S (S Z))))))) (gtypes f s)
{-# INLINE gtypes' #-}


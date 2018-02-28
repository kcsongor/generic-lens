{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
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
  , HasTypesDeep (..)
  ) where

import Data.Generics.Product.Internal.Types

import GHC.Generics
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

class HasTypesDeep a s where
  typesDeep :: Traversal' s a

  default typesDeep :: Traversal' s a
  typesDeep _ = pure

instance
  ( GHasTypesDeep a (Rep s)
  , Generic s
  ) => HasTypesDeep a s where
  typesDeep f s = to <$> gtypesDeep f (from s)

instance {-# OVERLAPPING #-} HasTypesDeep a Bool
instance {-# OVERLAPPING #-} HasTypesDeep a Char
instance {-# OVERLAPPING #-} HasTypesDeep a Double
instance {-# OVERLAPPING #-} HasTypesDeep a Float
instance {-# OVERLAPPING #-} HasTypesDeep a Int
instance {-# OVERLAPPING #-} HasTypesDeep a Integer
instance {-# OVERLAPPING #-} HasTypesDeep a Ordering

--------------------------------------------------------------------------------

class GHasTypesDeep a s where
  gtypesDeep :: Traversal' (s x) a

instance
  ( GHasTypesDeep a l
  , GHasTypesDeep a r
  ) => GHasTypesDeep a (l :*: r) where
  gtypesDeep f (l :*: r) = (:*:) <$> gtypesDeep f l <*> gtypesDeep f r
  {-# INLINE gtypesDeep #-}

instance
  ( GHasTypesDeep a l
  , GHasTypesDeep a r
  ) => GHasTypesDeep a (l :+: r) where
  gtypesDeep f (L1 l) = L1 <$> gtypesDeep f l
  gtypesDeep f (R1 r) = R1 <$> gtypesDeep f r
  {-# INLINE gtypesDeep #-}

instance (GHasTypesDeep a s) => GHasTypesDeep a (M1 m meta s) where
  gtypesDeep f (M1 s) = M1 <$> gtypesDeep f s
  {-# INLINE gtypesDeep #-}

instance {-# OVERLAPPING #-} GHasTypesDeep a (Rec0 a) where
  gtypesDeep f (K1 x) = K1 <$> f x
  {-# INLINE gtypesDeep #-}

instance HasTypesDeep a b => GHasTypesDeep a (Rec0 b) where
  gtypesDeep f (K1 x) = K1 <$> typesDeep @a f x
  {-# INLINE gtypesDeep #-}

instance GHasTypesDeep a U1 where
  gtypesDeep _ _ = pure U1
  {-# INLINE gtypesDeep #-}

--instance GHasTypesDeep a V1 where

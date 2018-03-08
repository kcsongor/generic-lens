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
import Data.Kind
import Data.Type.Bool

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
  {-# INLINE typesDeep #-}

instance
  ( HasTypesDeep1 (Interesting s a) a s
  ) => HasTypesDeep a s where
  typesDeep = typesDeep1 @(Interesting s a)
  {-# INLINE typesDeep #-}

class HasTypesDeep1 (t :: Bool) a s where
  typesDeep1 :: Traversal' s a

instance
  ( GHasTypesDeep a (Rep s)
  , Generic s
  ) => HasTypesDeep1 'True a s where
  typesDeep1 f s = to <$> gtypesDeep f (from s)
  --{-# INLINE typesDeep1 #-}

instance HasTypesDeep1 'False a s where
  typesDeep1 _ = pure
  --{-# INLINE typesDeep1 #-}

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

type Interesting f a = Interesting' f (Rep f) a

type family Interesting' orig f (a :: Type) :: Bool where
  Interesting' orig (M1 _ m f) t
    = Interesting' orig f t
  Interesting' orig (l :*: r) t
    = Interesting' orig l t || Interesting' orig r t
  Interesting' orig (l :+: r) t
    = Interesting' orig l t || Interesting' orig r t
  Interesting' orig (Rec0 t) t
    = 'True
  Interesting' _ (Rec0 Bool)     _ = 'False
  Interesting' _ (Rec0 Char)     _ = 'False
  Interesting' _ (Rec0 Double)   _ = 'False
  Interesting' _ (Rec0 Float)    _ = 'False
  Interesting' _ (Rec0 Int)      _ = 'False
  Interesting' _ (Rec0 Integer)  _ = 'False
  Interesting' _ (Rec0 Ordering) _ = 'False
  Interesting' orig (Rec0 orig) _
    = 'False
  Interesting' _ (Rec0 _) _
    = 'True
  Interesting' _ _ _
    = 'False

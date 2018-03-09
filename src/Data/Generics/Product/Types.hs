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
  ) where

import Data.Kind
import Data.Type.Bool

import GHC.Generics
import Data.Generics.Internal.VL.Traversal

class HasTypes a s where
  types :: Traversal' s a

  default types :: Traversal' s a
  types _ = pure
  {-# INLINE types #-}

instance
  ( HasTypes1 (Interesting s a) a s
  ) => HasTypes a s where
  types = types1 @(Interesting s a)
  {-# INLINE types #-}

class HasTypes1 (t :: Bool) a s where
  types1 :: Traversal' s a

instance
  ( GHasTypes a (Rep s)
  , Generic s
  ) => HasTypes1 'True a s where
  types1 f s = to <$> gtypes f (from s)
  --{-# INLINE types1 #-}

instance HasTypes1 'False a s where
  types1 _ = pure
  --{-# INLINE types1 #-}

instance {-# OVERLAPPING #-} HasTypes a Bool
instance {-# OVERLAPPING #-} HasTypes a Char
instance {-# OVERLAPPING #-} HasTypes a Double
instance {-# OVERLAPPING #-} HasTypes a Float
instance {-# OVERLAPPING #-} HasTypes a Int
instance {-# OVERLAPPING #-} HasTypes a Integer
instance {-# OVERLAPPING #-} HasTypes a Ordering

--------------------------------------------------------------------------------

class GHasTypes a s where
  gtypes :: Traversal' (s x) a

instance
  ( GHasTypes a l
  , GHasTypes a r
  ) => GHasTypes a (l :*: r) where
  gtypes f (l :*: r) = (:*:) <$> gtypes f l <*> gtypes f r
  {-# INLINE gtypes #-}

instance
  ( GHasTypes a l
  , GHasTypes a r
  ) => GHasTypes a (l :+: r) where
  gtypes f (L1 l) = L1 <$> gtypes f l
  gtypes f (R1 r) = R1 <$> gtypes f r
  {-# INLINE gtypes #-}

instance (GHasTypes a s) => GHasTypes a (M1 m meta s) where
  gtypes f (M1 s) = M1 <$> gtypes f s
  {-# INLINE gtypes #-}

instance {-# OVERLAPPING #-} GHasTypes a (Rec0 a) where
  gtypes f (K1 x) = K1 <$> f x
  {-# INLINE gtypes #-}

instance HasTypes a b => GHasTypes a (Rec0 b) where
  gtypes f (K1 x) = K1 <$> types @a f x
  {-# INLINE gtypes #-}

instance GHasTypes a U1 where
  gtypes _ _ = pure U1
  {-# INLINE gtypes #-}

--instance GHasTypes a V1 where

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

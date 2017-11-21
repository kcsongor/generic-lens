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
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE PolyKinds  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.Types
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Types
  ( GHasTypes (..)
  , showAp
  , runAp
  , Fuel(..)
  , Flatten(..)
  ) where

--import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics
import Control.Applicative ( liftA2)
import Debug.Trace

data Fuel = Z | S Fuel

class Flatten (n :: Fuel) where
  flatten :: Applicative f => Ap f a -> f a

instance Flatten Z where
  flatten a = runAp a
  {-# INLINE flatten #-}

type Ten = 'S ('S ('S Z))

instance Flatten n => Flatten ('S n) where
  flatten (Pure a) = pure a
  flatten (Comp a) = a
  flatten (App l r) = flatten @(n) (rewrite @Ten l) <*> flatten @n (rewrite @Ten r)
  flatten (FMap f ab) = f <$> flatten @n (rewrite @Ten ab)
  {-# INLINE flatten #-}

class Rewrite (n :: Fuel) where
  rewrite :: Ap f a -> Ap f a

instance Rewrite Z where
  rewrite a = rewrite' a
  {-# INLINE rewrite #-}

instance Rewrite n => Rewrite ('S n) where
  rewrite a = rewrite @n (rewrite' a)
  {-# INLINE rewrite #-}

data Ap f a where
  Pure :: a -> Ap f a
  Comp :: f a -> Ap f a
  App :: Ap f (a -> b) -> Ap f a -> Ap f b
  FMap :: (a -> b) -> Ap f a -> Ap f b

showAp :: Ap f a -> String
showAp (Pure a) = "Pure"
showAp (App fa afb) = "(Ap fa"

traceAp :: Ap f a -> Ap f a
traceAp a = trace (showAp a) a

instance Functor (Ap f) where
  fmap = FMap

instance Applicative (Ap f) where
  pure = Pure
  (<*>) = App
--  Ap x y <*> z = Ap x (flip <$> y <*> z)

runAp :: Applicative f => Ap f a -> f a
runAp (Pure x) = pure x
runAp (App fa fab) = runAp fa <*> runAp fab
runAp (FMap f ab) = fmap f (runAp ab)
runAp (Comp x) = x

-- u <*> (v <*> w) => pure (.) <*> u <*> v <*> w
rewrite' :: Ap f a -> Ap f a
rewrite' (App u (App v w)) = App (App (App (Pure (.)) u) v) w
rewrite' (App u (FMap f' fa)) = App (FMap (\f a2 -> f (f' a2))  u) fa
rewrite' ap = ap


-- |As 'HasTypes' but over generic representations as defined by
--  "GHC.Generics".
class GHasTypes (f :: Type -> Type) a where
--  gtypes :: Traversal' (f x) a
  gtypes :: forall g x . (a -> g a) -> f x -> Ap g (f x)

instance (GHasTypes l a, GHasTypes r a) => GHasTypes (l :*: r) a where
  gtypes f (l :*: r) =  liftA2 (:*:) (gtypes f l) (gtypes f r)
  {-# INLINE gtypes #-}

-- TODO:
-- instance (GHasTypes l a, GHasTypes r a) => GHasTypes (l :+: r) a where

instance GHasTypes (K1 R a) a where
  gtypes f (K1 x) = App (Pure K1) (Comp (f x))

instance {-# OVERLAPS #-} GHasTypes (K1 R a) b where
  gtypes _ k = Pure k

instance GHasTypes f a => GHasTypes (M1 m meta f) a where
  gtypes = mIso . gtypes

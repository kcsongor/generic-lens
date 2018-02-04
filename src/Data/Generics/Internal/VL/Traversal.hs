{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.VL.Traversal
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.VL.Traversal where

-- | Type alias for traversal
type Traversal' s a
  = forall f. Applicative f => (a -> f a) -> s -> f s

type TraversalC' s c
  = forall f. Applicative f => LensLikeC f s c

type LensLikeC f s c
  = (forall a. c a => a -> f a) -> s -> f s

type Traversal s t a b
  = forall f. Applicative f => (a -> f b) -> s -> f t

confusing :: Applicative f => Traversal s t a b -> (a -> f b) -> s -> f t
confusing t = \f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
{-# INLINE confusing #-}

-- fuse constrained traversals
confusingC :: forall c f s. Applicative f => TraversalC' s c -> LensLikeC f s c
confusingC t = \f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
{-# INLINE confusingC #-}

liftCurriedYoneda :: Applicative f => f a -> Curried (Yoneda f) a
liftCurriedYoneda fa = Curried (`yap` fa)
{-# INLINE liftCurriedYoneda #-}

yap :: Applicative f => Yoneda f (a -> b) -> f a -> Yoneda f b
yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa)
{-# INLINE yap #-}

newtype Curried f a =
  Curried { runCurried :: forall r. f (a -> r) -> f r }

instance Functor f => Functor (Curried f) where
  fmap f (Curried g) = Curried (g . fmap (.f))
  {-# INLINE fmap #-}

instance (Functor f) => Applicative (Curried f) where
  pure a = Curried (fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}

liftCurried :: Applicative f => f a -> Curried f a
liftCurried fa = Curried (<*> fa)

lowerCurried :: Applicative f => Curried f a -> f a
lowerCurried (Curried f) = f (pure id)
newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda a = Yoneda (\f -> fmap f a)

lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda f) = f id

instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)

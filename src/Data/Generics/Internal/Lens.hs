{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.Lens
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Lens where

import Control.Applicative  ( Const (..) )
import GHC.Generics         ( (:*:) (..), Generic (..), M1 (..), Rep, (:+:) (..) )
import Data.Profunctor      ( dimap, Choice (..) )

-- | Identity functor
newtype Identity a
  = Identity { runIdentity :: a }

-- | Functor instance
instance Functor Identity where
  fmap f (Identity a)
    = Identity (f a)

-- | Type alias for lens
type Lens' s a
  = forall f. Functor f => (a -> f a) -> s -> f s

type Prism' s a
  = forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)

-- | Getting
(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)

-- | Setting
set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set l b
  = runIdentity . l (\_ -> Identity b)

-- | Lens focusing on the first element of a product
first :: Lens' ((a :*: b) x) (a x)
first f (a :*: b)
  = fmap (:*: b) (f a)

-- | Lens focusing on the second element of a product
second :: Lens' ((a :*: b) x) (b x)
second f (a :*: b)
  = fmap (a :*:) (f b)

prism :: (a -> s) -> (s -> Either s a) -> Prism' s a
prism bt seta = dimap seta (either pure (fmap bt)) . right'

either' :: (f x -> c) -> (g x -> c) -> (:+:) f g x -> c
either' f _ (L1 x) = f x
either' _ g (R1 x) = g x

firstMaybe :: Prism' ((a :+: b) x) (a x)
firstMaybe = prism L1 (either' Right (Left . R1))

secondMaybe :: Prism' ((a :+: b) x) (b x)
secondMaybe = prism R1 (either' (Left . L1) Right)

combine :: Lens' (s x) a -> Lens' (t x) a -> Lens' ((:+:) s t x) a
combine sa _ f (L1 s) = fmap (\a -> L1 (set sa a s)) (f (s ^. sa))
combine _ ta f (R1 t) = fmap (\a -> R1 (set ta a t)) (f (t ^. ta))

-- | A type and its generic representation are isomorphic
repIso :: Generic a => Lens' a (Rep a x)
repIso a = fmap to . a . from

-- | 'M1' is just a wrapper around `f p`
lensM :: Lens' (M1 i c f p) (f p)
lensM f (M1 x) = fmap M1 (f x)

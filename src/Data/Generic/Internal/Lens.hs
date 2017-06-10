{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generic.Internal.Lens
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generic.Internal.Lens where

import Control.Applicative  ( Const (..) )
import GHC.Generics         ( (:*:) (..), Generic (..), M1 (..), Rep )

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

-- | A type and its generic representation are isomorphic
repIso :: Generic a => Lens' a (Rep a x)
repIso a = fmap to . a . from

-- | 'M1' is just a wrapper around `f p`
lensM :: Lens' (M1 i c f p) (f p)
lensM f (M1 x) = fmap M1 (f x)

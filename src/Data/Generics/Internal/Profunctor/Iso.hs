{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.Profunctor.Iso
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Profunctor.Iso where

import Data.Profunctor        (Profunctor(..))
import Data.Profunctor.Unsafe ((#.), (.#))
import GHC.Generics           ((:*:)(..), (:+:)(..), Generic(..), M1(..), K1(..), Rep)
import Data.Coerce

type Iso s t a b
  = forall p. (Profunctor p) => p a b -> p s t

type Iso' s a = Iso s s a a

-- | A type and its generic representation are isomorphic
repIso :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b x)
repIso = iso from to

-- | 'M1' is just a wrapper around `f p`
--mIso :: Iso' (M1 i c f p) (f p)
mIso :: Iso (M1 i c f p) (M1 i c g p) (f p) (g p)
mIso = iso unM1 M1
{-# INLINE mIso #-}

kIso :: Iso (K1 r a p) (K1 r b p) a b
kIso = iso unK1 K1
{-# INLINE kIso #-}

sumIso :: Iso ((a :+: b) x) ((a' :+: b') x) (Either (a x) (b x)) (Either (a' x) (b' x))
sumIso = iso back forth
  where forth (Left l)  = L1 l
        forth (Right r) = R1 r
        back (L1 l) = Left l
        back (R1 r) = Right r

prodIso :: Iso ((a :*: b) x) ((a' :*: b') x) (a x, b x) (a' x, b' x)
prodIso = iso (\(a :*: b) -> (a, b)) (\(a, b) -> (a :*: b))

assoc3 :: Iso ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3 = iso (\((a, b), c) -> (a, (b, c))) (\(a, (b, c)) -> ((a, b), c))

--------------------------------------------------------------------------------
-- Iso stuff

fromIso :: Iso s t a b -> Iso b a t s
fromIso l = withIso l $ \ sa bt -> iso bt sa
{-# INLINE fromIso #-}

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap
{-# INLINE iso #-}

withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id id) of
  Exchange sa bt -> k sa bt

pairing :: Iso s t a b -> Iso s' t' a' b' -> Iso (s, s') (t, t') (a, a') (b, b')
pairing f g = withIso f $ \ sa bt -> withIso g $ \s'a' b't' ->
  iso (bmap sa s'a') (bmap bt b't')
  where bmap f' g' (a, b) = (f' a, g' b)

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE rmap #-}
  ( #. ) _ = coerce
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = coerce p
  {-# INLINE ( .# ) #-}


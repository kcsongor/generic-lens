{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.Profunctor.Prism
-- Copyright   :  (C) 2019 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Profunctor.Prism where

import Data.Bifunctor         (bimap)
import Data.Profunctor        (Choice(..), Profunctor(..))
import Data.Tagged
import Data.Profunctor.Unsafe ((#.), (.#))
import GHC.Generics
import Data.Coerce

type APrism s t a b = Market a b a b -> Market a b s t

type Prism s t a b
  = forall p . (Choice p) => p a b -> p s t

type Prism' s a = forall p . (Choice p) => p a a -> p s s

left :: Prism ((a :+: c) x) ((b :+: c) x) (a x) (b x)
left = prism L1 $ gsum Right (Left . R1)

right :: Prism ((a :+: b) x) ((a :+: c) x) (b x) (c x)
right = prism R1 $ gsum (Left . L1) Right

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta eta = dimap seta (either id bt) (right' eta)

_Left :: Prism (Either a c) (Either b c) a b
_Left = left'

_Right :: Prism (Either c a) (Either c b) a b
_Right = right'

prismPRavel :: APrism s t a b -> Prism s t a b
prismPRavel l pab = (prism2prismp $ l idPrism) pab

build :: (Tagged b b -> Tagged t t) -> b -> t
build p = unTagged #. p .# Tagged

match :: Prism s t a b -> s -> Either t a
match k = withPrism k $ \_ _match -> _match

--------------------------------------------------------------------------------
-- Prism stuff

without' :: Prism s t a b -> Prism s t c d -> Prism s t (Either a c) (Either b d)
without' k p =
  (withPrism k  $ \bt _ k' ->
   withPrism k' $ \dt setc ->
     prism (either bt dt) $ \s -> fmap Right (setc s)) p
{-# INLINE without' #-}

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case k idPrism of
  Market bt seta -> f bt seta

prism2prismp :: Market a b s t -> Prism s t a b
prism2prismp (Market bt seta) = prism bt seta

idPrism :: Market a b a b
idPrism = Market id Right

gsum :: (a x -> c) -> (b x -> c) -> ((a :+: b) x) -> c
gsum f _ (L1 x) =  f x
gsum _ g (R1 y) =  g y

plus :: (a -> b) -> (c -> d) -> Either a c -> Either b d
plus = bimap

--------------------------------------------------------------------------------
-- Market

data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
  lmap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE lmap #-}
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE rmap #-}
  ( #. ) _ = coerce
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = coerce p
  {-# INLINE ( .# ) #-}

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \case
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  {-# INLINE left' #-}

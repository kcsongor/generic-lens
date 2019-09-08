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
module Data.Generics.Internal.Profunctor.Prism
  ( Prism
  , Prism'
  , left
  , right
  , prism
  , _Left
  , _Right
  , prismPRavel
  , build
  , match
  , without'
  , withPrism
  , prism2prismp
  , idPrism
  , gsum
  , plus

  -- re-exports
  , Market (..)
  ) where

import Data.Bifunctor         (bimap)
import Optics.Internal.Profunctor        (Choice(..), Profunctor(..))
import Optics.Internal.Tagged
import Optics.Internal.Concrete (Market (..))
import GHC.Generics

import Optics.Internal.Utils

type APrism i s t a b = Market a b i a b -> Market a b i s t

type Prism s t a b
  = forall p i . (Choice p) => p i a b -> p i s t

type Prism' s a = forall i p . (Choice p) => p i a a -> p i s s

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

prismPRavel :: APrism i s t a b -> Prism s t a b
prismPRavel l pab = (prism2prismp $ l idPrism) pab

build :: (Tagged i b b -> Tagged i t t) -> b -> t
build p = unTagged #. p .# Tagged

match :: Prism s t a b -> s -> Either t a
match k = withPrism k $ \_ _match -> _match

--------------------------------------------------------------------------------
-- Prism stuff

without' :: Prism s t a b -> Prism s t c d -> Prism s t (Either a c) (Either b d)
without' k =
  withPrism k  $ \bt _ k' ->
  withPrism k' $ \dt setc ->
    prism (either bt dt) $ \s -> fmap Right (setc s)
{-# INLINE without' #-}

withPrism :: APrism i s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case k idPrism of
  Market bt seta -> f bt seta

prism2prismp :: Market a b i s t -> Prism s t a b
prism2prismp (Market bt seta) = prism bt seta

idPrism :: Market a b i a b
idPrism = Market id Right

gsum :: (a x -> c) -> (b x -> c) -> ((a :+: b) x) -> c
gsum f _ (L1 x) =  f x
gsum _ g (R1 y) =  g y

plus :: (a -> b) -> (c -> d) -> Either a c -> Either b d
plus = bimap

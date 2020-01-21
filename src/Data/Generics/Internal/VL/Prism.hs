{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.VL.Prism
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.VL.Prism where

import Data.Functor.Identity  (Identity(..))
import Data.Profunctor        (Choice(..), Profunctor(..))
import Data.Coerce
import Data.Generics.Internal.Profunctor.Prism (Market (..), plus, idPrism)
import Data.Tagged
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Monoid            (First (..))
import Control.Applicative    (Const(..))

-- | Type alias for prism
type Prism s t a b
  = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a
  = Prism s s a a

infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)


match :: Prism s t a b -> s -> Either t a
match k = withPrism k $ \_ _match -> _match
{-# INLINE match #-}

(#) :: (Tagged b (Identity b) -> Tagged t (Identity t)) -> b -> t
(#) = build
{-# INLINE (#) #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta eta = dimap (\x -> plus pure id (seta x)) (either id (\x -> fmap bt x)) (right' eta)
{-# INLINE prism #-}

prismRavel :: (Market a b a b -> Market a b s t) -> Prism s t a b
prismRavel l pab  = (prism2prismvl $ l idPrism) pab
{-# INLINE prismRavel #-}

type APrismVL s t a b = Market a b a (Identity b) -> Market a b s (Identity t)

withPrism :: APrismVL s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case coerce (k (Market Identity Right)) of
                  Market bt seta -> f bt seta

prism2prismvl :: Market a b s t -> Prism s t a b
prism2prismvl  (Market bt seta) = prism bt seta
{-# INLINE prism2prismvl #-}

build :: (Tagged b (Identity b) -> Tagged t (Identity t)) -> b -> t
build p = runIdentity #. unTagged #. p .# Tagged .# Identity
{-# INLINE build #-}

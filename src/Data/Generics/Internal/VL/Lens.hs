{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.VL.Lens
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.VL.Lens where

import Control.Applicative    (Const(..))
import Data.Functor.Identity  (Identity(..))
import Data.Generics.Internal.Profunctor.Lens (ALens (..), (^.), idLens)

-- | Type alias for lens
type Lens' s a
  = Lens s s a a

type Lens s t a b
  = forall f. Functor f => (a -> f b) -> s -> f t

view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l s = (^.) s l

set :: Lens s t a b -> b -> s -> t
set l x = runIdentity . (l (Identity . const x))

lens2lensvl :: ALens a b s t -> Lens s t a b
lens2lensvl (ALens _get _set) = lens _get _set

ravel :: (ALens a b a b -> ALens a b s t)
      ->  Lens s t a b
ravel l pab = (lens2lensvl $ l idLens) pab

lens :: (s -> a) -> ((s, b) -> t) -> Lens s t a b
lens get _set f x = curry _set x <$> f (get x)


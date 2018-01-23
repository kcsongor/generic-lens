{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TupleSections             #-}

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
import Data.Generics.Internal.Profunctor.Lens (ALens (..), idLens)

-- | Type alias for lens
type Lens' s a
  = Lens s s a a

type Lens s t a b
  = forall f. Functor f => (a -> f b) -> s -> f t

view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l s = (^.) s l

-- | Getting
(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)
infixl 8 ^.

infixr 4 .~
(.~) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
(.~) f b = runIdentity . f (Identity . const b)

set :: Lens s t a b -> b -> s -> t
set l x = l .~ x

lens2lensvl :: ALens a b s t -> Lens s t a b
lens2lensvl (ALens _get _set) =
  \f x ->
    case _get x of
      (c, a) -> (_set . (c, )) <$> f a
{-# INLINE lens2lensvl #-}

ravel :: (ALens a b a b -> ALens a b s t)
      ->  Lens s t a b
ravel l pab = (lens2lensvl $ l idLens) pab


lens :: (s -> a) -> ((s, b) -> t) -> Lens s t a b
lens get _set = \f x -> curry _set x <$> f (get x)
{-# INLINE[0] lens #-}


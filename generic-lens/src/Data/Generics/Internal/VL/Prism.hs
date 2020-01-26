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

import Data.Functor.Identity (Identity (..))
import qualified Data.Generics.Internal.Profunctor.Prism as P
import qualified Data.Profunctor as P
import Data.Coerce (coerce)

-- | Type alias for prism
type Prism s t a b
  = forall p f. (P.Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a
  = Prism s s a a

match :: Prism s t a b -> s -> Either t a
match p = case p (Market Identity Right) of
  Market _ seta -> coerce seta
{-# INLINE match #-}

build :: Prism s t a b -> b -> t
build p = case p (Market Identity Right) of
  Market bt _ -> coerce bt
{-# INLINE build #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta eta = P.dimap (\x -> P.left' pure (seta x)) (either id (\x -> fmap bt x)) (P.right' eta)
{-# INLINE prism #-}

prism2prismvl :: P.APrism i s t a b -> Prism s t a b
prism2prismvl  _prism = P.withPrism _prism prism
{-# INLINE prism2prismvl #-}

--------------------------------------------------------------------------------
-- Market

data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance P.Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
  lmap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE lmap #-}
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE rmap #-}

instance P.Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  {-# INLINE left' #-}
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE right' #-}

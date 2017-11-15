{-# LANGUAGE GADTs         #-}
{-# LANGUAGE Rank2Types    #-}
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

import Control.Applicative   (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Profunctor       (Choice(right'), Profunctor(dimap))
import GHC.Generics          ((:*:)(..), (:+:)(..), Generic(..), M1(..), Rep)

-- | Type alias for lens
type Lens' s a
  = forall f. Functor f => (a -> f a) -> s -> f s

-- | Type alias for traversal
type Traversal' s a
  = forall f. Applicative f => (a -> f a) -> s -> f s

-- | Type alias for prism
type Prism' s a
  = forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)

type Iso' s a
  = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

-- | Getting
(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)
infixl 8 ^.

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

left :: Prism' ((a :+: b) x) (a x)
left = prism L1 $ \x -> case x of
  L1 a -> Right a
  R1 _ -> Left x

right :: Prism' ((a :+: b) x) (b x)
right = prism R1 $ \x -> case x of
  L1 _ -> Left x
  R1 a -> Right a

combine :: Lens' (s x) a -> Lens' (t x) a -> Lens' ((s :+: t) x) a
combine sa _ f (L1 s) = fmap (\a -> L1 (set sa a s)) (f (s ^. sa))
combine _ ta f (R1 t) = fmap (\a -> R1 (set ta a t)) (f (t ^. ta))

prism :: (a -> s) -> (s -> Either s a) -> Prism' s a
prism bt seta = dimap seta (either pure (fmap bt)) . right'

-- | A type and its generic representation are isomorphic
repIso :: Generic a => Iso' a (Rep a x)
repIso = dimap from (fmap to)

-- | 'M1' is just a wrapper around `f p`
mIso :: Iso' (M1 i c f p) (f p)
mIso = dimap unM1 (fmap M1)

-- These are specialised versions of the Isos above. On GHC 8.0.2, having
-- these functions eta-expanded allows the optimiser to inline these functions.
mLens :: Lens' (M1 i c f p) (f p)
mLens f s = mIso f s

repLens :: Generic a => Lens' a (Rep a x)
repLens f s = repIso f s

sumIso :: Iso' ((a :+: b) x) (Either (a x) (b x))
sumIso = dimap f (fmap t)
  where f (L1 x) = Left x
        f (R1 x) = Right x
        t (Left x) = L1 x
        t (Right x) = R1 x

_Left :: Prism' (Either a c) a
_Left = prism Left $ either Right (Left . Right)

_Right :: Prism' (Either c a) a
_Right = prism Right $ either (Left . Left) Right

--------------------------------------------------------------------------------

data Coyoneda f b = forall a. Coyoneda (a -> b) (f a)

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g fa)
    = Coyoneda (f . g) fa

instance Applicative f => Applicative (Coyoneda f) where
  pure = proj . pure
  Coyoneda f fa <*> Coyoneda f' fa' =
    proj $ (\a1 a2 -> f a1 (f' a2)) <$> fa <*> fa'

inj :: Functor f => Coyoneda f a -> f a
inj (Coyoneda f a) = fmap f a

proj :: Functor f => f a -> Coyoneda f a
proj fa = Coyoneda id fa

ravel :: Functor f => ((a -> Coyoneda f b) -> s -> Coyoneda f t) -> (a -> f b) -> (s -> f t)
ravel coy f s = inj $ coy (\a -> proj (f a)) s

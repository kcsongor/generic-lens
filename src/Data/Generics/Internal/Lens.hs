{-# LANGUAGE LambdaCase #-}
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

import Control.Applicative    (Const(..))
import Data.Functor.Identity  (Identity(..))
import Data.Monoid            (First (..))
import Data.Profunctor        (Choice(right'), Profunctor(dimap))
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Tagged
import GHC.Generics           ((:*:)(..), (:+:)(..), Generic(..), M1(..), K1(..), Rep)

-- | Type alias for lens
type Lens' s a
  = Lens s s a a

type Lens s t a b
  = forall f. Functor f => (a -> f b) -> s -> f t

-- | Type alias for prism
type Prism s t a b
  = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a
  = Prism s s a a

type Iso' s a
  = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

type Iso s t a b
  = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | Getting
(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)
infixl 8 ^.

-- | Setting
set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set l b
  = runIdentity . l (\_ -> Identity b)

infixr 4 .~
(.~) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
(.~) = set

infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)

infixr 8 #
(#) :: (Tagged b (Identity b) -> Tagged t (Identity t)) -> b -> t
(#) p = runIdentity #. unTagged #. p .# Tagged .# Identity

-- | Lens focusing on the first element of a product
first :: Lens ((a :*: b) x) ((a' :*: b) x) (a x) (a' x)
first f (a :*: b)
  = fmap (:*: b) (f a)

-- | Lens focusing on the second element of a product
second :: Lens ((a :*: b) x) ((a :*: b') x) (b x) (b' x)
second f (a :*: b)
  = fmap (a :*:) (f b)

left :: Prism ((a :+: c) x) ((b :+: c) x) (a x) (b x)
left = prism L1 $ gsum Right (Left . R1)

right :: Prism ((a :+: b) x) ((a :+: c) x) (b x) (c x)
right = prism R1 $ gsum (Left . L1) Right

gsum :: (a x -> c) -> (b x -> c) -> ((a :+: b) x) -> c
gsum f _ (L1 x) =  f x
gsum _ g (R1 y) =  g y

combine :: Lens' (s x) a -> Lens' (t x) a -> Lens' ((s :+: t) x) a
combine sa _ f (L1 s) = fmap (\a -> L1 (set sa a s)) (f (s ^. sa))
combine _ ta f (R1 t) = fmap (\a -> R1 (set ta a t)) (f (t ^. ta))

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

-- | A type and its generic representation are isomorphic
repIso :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b x)
repIso = dimap from (fmap to)

-- | 'M1' is just a wrapper around `f p`
--mIso :: Iso' (M1 i c f p) (f p)
mIso :: Iso (M1 i c f p) (M1 i c g p) (f p) (g p)
mIso = dimap unM1 (fmap M1)

kIso :: Iso (K1 r a p) (K1 r b p) a b
kIso = dimap unK1 (fmap K1)

-- These are specialised versions of the Isos above. On GHC 8.0.2, having
-- these functions eta-expanded allows the optimiser to inline these functions.
mLens :: Lens (M1 i c f p) (M1 i c g p) (f p) (g p)
mLens f s = mIso f s

kLens :: Lens (K1 r a p) (K1 r b p) a b
kLens f s = kIso f s

repLens :: (Generic a, Generic b) => Lens a b (Rep a x) (Rep b x)
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

inj :: Functor f => Coyoneda f a -> f a
inj (Coyoneda f a) = fmap f a

proj :: Functor f => f a -> Coyoneda f a
proj fa = Coyoneda id fa

ravel :: Functor f => ((a -> Coyoneda f b) -> s -> Coyoneda f t) -> (a -> f b) -> (s -> f t)
ravel coy f s = inj $ coy (\a -> proj (f a)) s

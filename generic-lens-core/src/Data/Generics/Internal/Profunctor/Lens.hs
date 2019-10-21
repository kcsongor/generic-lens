{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.Profunctor.Lens
-- Copyright   :  (C) 2019 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Profunctor.Lens where

import Data.Profunctor.Indexed        (Profunctor(..), Strong(..), FunArrow (..))
import Data.Bifunctor
import GHC.Generics
import Data.Generics.Internal.Profunctor.Iso

type Lens s t a b
  = forall i p. (Strong p) => p i a b -> p i s t

type LensLike i p s t a b
  = p i a b -> p i s t


ravel :: (ALens a b i a b -> ALens a b i s t) -> Lens s t a b
ravel l pab = conv (l idLens) pab
  where
    conv :: ALens a b i s t -> Lens s t a b
    conv (ALens _get _set) = lens' _get _set

over :: Lens s t a b -> (a -> b) -> s -> t
over o = \f -> runFunArrow $ o (FunArrow f)
{-# INLINE over #-}

-- | Setting
set :: Lens s t a b -> b -> s -> t
set o = over o . const
{-# INLINE set #-}

view :: Lens s s a a -> s -> a
view l = withLensPrim l (\get _ -> snd . get)

withLensPrim :: Lens s t a b -> (forall c . (s -> (c,a)) -> ((c, b) -> t) -> r) -> r
withLensPrim l k =
 case l idLens of
   ALens _get _set -> k _get _set

idLens :: ALens a b i a b
idLens = ALens (fork (const ()) id) snd
{-# INLINE idLens #-}

-- | Lens focusing on the first element of a product
first :: Lens ((a :*: b) x) ((a' :*: b) x) (a x) (a' x)
first
  = lens' (\(a :*: b) -> (b,a)) (\(b, a') -> a' :*: b)

-- | Lens focusing on the second element of a product
second :: Lens ((a :*: b) x) ((a :*: b') x) (b x) (b' x)
second
  = lens' (\(a :*: b) -> (a,b)) (\(a, b') -> a :*: b')

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g a = (f a, g a)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross = bimap

--------------------------------------------------------------------------------

data Coyoneda f b = forall a. Coyoneda (a -> b) (f a)

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g fa)
    = Coyoneda (f . g) fa

inj :: Functor f => Coyoneda f a -> f a
inj (Coyoneda f a) = fmap f a

proj :: Functor f => f a -> Coyoneda f a
proj fa = Coyoneda id fa

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

-- Could implement this using primitives?
alongside :: Lens s t a  b
          -> Lens s' t' a' b'
          -> Lens (s, s') (t, t') (a, a') (b, b')
alongside l r = withLensPrim l $ \getl setl ->
                withLensPrim r $ \getr setr ->
  lens (\(s, s') -> (snd (getl s), snd (getr s')))
       (\(s, s') (b, b') -> (setl (fst (getl s), b), setr (fst (getr s'), b')))

assoc3L :: Lens ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3L f = assoc3 f

stron :: (Either s s', b) -> Either (s, b) (s', b)
stron (e, b) =  bimap (,b) (, b) e

choosing :: Lens s t a b -> Lens s' t' a b -> Lens (Either s s') (Either t t') a b
choosing l r = withLensPrim l $ \getl setl ->
               withLensPrim r $ \getr setr ->
                            let
                                g e = case e of
                                        Left v -> let (c, v') = getl v in (Left c, v')
                                        Right v -> let (c, v') = getr v in (Right c, v')
                                s = bimap setl setr . stron
                            in lens' g s

lens' :: (s -> (c,a)) -> ((c,b) -> t) -> Lens s t a b
lens' get _set = dimap get _set . second'
{-# INLINE lens' #-}

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get _set = dimap (\s -> (get s, s)) (\(b, s) -> _set s b) . first'
{-# INLINE lens #-}

------------------------------------------------------------------------------

data ALens a b i s t = forall c . ALens (s -> (c,a)) ((c, b) -> t)

instance Functor (ALens a b i s) where
  fmap f (ALens _get _set) = ALens _get (f . _set)

instance Profunctor (ALens a b) where
  dimap f g (ALens get _set) = ALens (get . f) (g . _set)
  lmap f = dimap f id
  rmap f = dimap id f

instance Strong (ALens a b) where
  first' = dimap swap swap . second'
  {-# INLINE first' #-}
  second' (ALens get _set) = ALens get' set'
    where
      get' (c, a1) = let (c1, a) = get a1 in ((c, c1), a)
      set' ((c, c1), b) = (c, _set (c1, b))
  {-# INLINE second' #-}

-- These are specialised versions of the Isos. On GHC 8.0.2, having
-- these functions eta-expanded allows the optimiser to inline these functions.
mLens :: Lens (M1 i c f p) (M1 i c g p) (f p) (g p)
mLens f = mIso f

repLens :: (Generic a, Generic b) => Lens a b (Rep a x) (Rep b x)
repLens f = repIso f

prodL :: Lens ((a :*: b) x) ((a' :*: b') x) (a x, b x) (a' x, b' x)
prodL f = prodIso f

prodR :: Lens (a' x, b' x) (a x, b x) ((a' :*: b') x) ((a :*: b) x)
prodR f = fromIso prodIso f

assoc3R :: Lens (a', (b', c')) (a, (b, c)) ((a', b'), c') ((a, b), c)
assoc3R f = fromIso assoc3 f

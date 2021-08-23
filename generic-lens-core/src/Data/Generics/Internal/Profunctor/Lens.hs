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
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Profunctor.Lens where

import Data.Profunctor.Indexed        (Profunctor(..), Strong(..))
import Data.Bifunctor
import GHC.Generics
import Data.Generics.Internal.Profunctor.Iso

type Lens s t a b
  = forall p i . Strong p => p i a b -> p i s t

type LensLike p s t a b
  = p a b -> p s t


ravel :: (ALens a b i a b -> ALens a b i s t) -> Lens s t a b
ravel l pab = conv (l idLens) pab
  where
    conv :: ALens a b i s t -> Lens s t a b
    conv (ALens _get _set) = lens _get _set
{-# INLINE ravel #-}

-- | Setting
set :: ((a -> b) -> s -> t) -> (s, b) -> t
set f (s, b)
  = f  (const b) s
{-# INLINE set #-}

view :: Lens s s a a -> s -> a
view l = withLensPrim l (\get _ -> snd . get)
{-# INLINE view #-}

--withLens :: Lens s t a b -> ((s -> a) -> ((s, b) -> t) -> r) -> r
--ithLens l k =
-- case l idLens of
--   ALens _get _set -> k (snd . _get) (\(s, b) -> _set ((fst $ _get s), b))

withLensPrim :: Lens s t a b -> (forall c . (s -> (c,a)) -> ((c, b) -> t) -> r) -> r
withLensPrim l k =
 case l idLens of
   ALens _get _set -> k _get _set
{-# INLINE withLensPrism #-}

idLens :: ALens a b i a b
idLens = ALens (fork (const ()) id) snd
{-# INLINE idLens #-}

-- | Lens focusing on the first element of a product
first :: Lens ((a :*: b) x) ((a' :*: b) x) (a x) (a' x)
first
  = lens (\(a :*: b) -> (b,a)) (\(b, a') -> a' :*: b)
{-# INLINE first #-}

-- | Lens focusing on the second element of a product
second :: Lens ((a :*: b) x) ((a :*: b') x) (b x) (b' x)
second
  = lens (\(a :*: b) -> (a,b)) (\(a, b') -> a :*: b')
{-# INLINE second #-}

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g a = (f a, g a)
{-# INLINE fork #-}

--------------------------------------------------------------------------------

data Coyoneda f b = forall a. Coyoneda (a -> b) (f a)

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g fa)
    = Coyoneda (f . g) fa
  {-# INLINE fmap #-}

inj :: Functor f => Coyoneda f a -> f a
inj (Coyoneda f a) = fmap f a
{-# INLINE inj #-}

proj :: Functor f => f a -> Coyoneda f a
proj fa = Coyoneda id fa
{-# INLINE proj #-}

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

assoc3L :: Lens ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3L f = assoc3 f
{-# INLINE assoc3L #-}

stron :: (Either s s', b) -> Either (s, b) (s', b)
stron (e, b) =  bimap (,b) (, b) e
{-# INLINE stron #-}

choosing :: forall s t a b s' t' . Lens s t a b -> Lens s' t' a b -> Lens (Either s s') (Either t t') a b
choosing l r = withLensPrim l (\getl setl ->
                  withLensPrim r (\getr setr ->
                            let --g :: Either s s' -> a
                                g e = case e of
                                        Left v -> let (c, v') = getl v in (Left c, v')
                                        Right v -> let (c, v') = getr v in (Right c, v')
                                s = bimap setl setr . stron
                            in lens g s))
{-# INLINABLE choosing #-}

lens :: (s -> (c,a)) -> ((c,b) -> t) -> Lens s t a b
lens get _set = dimap get _set . second'
{-# INLINE lens #-}

------------------------------------------------------------------------------

data ALens a b i s t = forall c . ALens (s -> (c,a)) ((c, b) -> t)

instance Functor (ALens a b i s) where
  fmap f (ALens _get _set) = ALens _get (f . _set)
  {-# INLINE fmap #-}
  
instance Profunctor (ALens a b) where
  dimap f g (ALens get _set) = ALens (get . f) (g . _set)
  {-# INLINE dimap #-}
  lmap f = dimap f id
  {-# INLINE lmap #-}
  rmap f = dimap id f
  {-# INLINE rmap #-}

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
{-# INLINE swap #-}

instance Strong (ALens a b) where
  first' = dimap swap swap . second'
  {-# INLINE first' #-}
  second' (ALens get _set) = ALens get' set'
    where
      get' (c, a1) = let (c1, a) = get a1 in ((c, c1), a)
      set' ((c, c1), b) = (c, _set (c1, b))
  {-# INLINE second' #-}

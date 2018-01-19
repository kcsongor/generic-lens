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
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Profunctor.Lens where

import Control.Applicative    (Const(..))
import Data.Monoid            (First (..))
import Data.Profunctor        (Profunctor(..), Strong(..))
import Data.Bifunctor
import Data.Profunctor.Unsafe ((#.))
import Data.Tagged
import GHC.Generics
import Data.Generics.Internal.Profunctor.Iso

type Lens s t a b
  = forall p . (Strong p) => p a b -> p s t

type LensLike p s t a b
  = p a b -> p s t

-- | Getting
(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)
infixl 8 ^.

-- | Setting
set :: ((a -> b) -> s -> t) -> (s, b) -> t
set f (s, b)
  = f  (const b) s

view :: Lens s s a a -> s -> a
view l = withLensPrim l (\get _ -> snd . get)

--withLens :: Lens s t a b -> ((s -> a) -> ((s, b) -> t) -> r) -> r
--ithLens l k =
-- case l idLens of
--   ALens _get _set -> k (snd . _get) (\(s, b) -> _set ((fst $ _get s), b))

withLensPrim :: Lens s t a b -> (forall c . (s -> (c,a)) -> ((c, b) -> t) -> r) -> r
withLensPrim l k =
 case l idLens of
   ALens _get _set -> k _get _set

idLens :: ALens a b a b
idLens = ALens (fork (const ()) id) snd
{-# INLINE idLens #-}

infixr 4 .~
(.~) :: ((a -> b) -> s -> t) -> b -> s -> t
(.~) f b s = set f (s, b)

infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)

build :: (Tagged b b -> Tagged t t) -> b -> t
build p = unTagged . p . Tagged


-- | Lens focusing on the first element of a product
first :: Lens ((a :*: b) x) ((a' :*: b) x) (a x) (a' x)
first
  = lens (\(a :*: b) -> (b,a)) (\(b, a') -> a' :*: b)

-- | Lens focusing on the second element of a product
second :: Lens ((a :*: b) x) ((a :*: b') x) (b x) (b' x)
second
  = lens (\(a :*: b) -> (a,b)) (\(a, b') -> a :*: b')

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

newtype Alongside p s t a b = Alongside { getAlongside :: p (s, a) (t, b) }

instance Profunctor p => Profunctor (Alongside p c d) where
  dimap f g (Alongside pab) = Alongside $ dimap (fmap f) (fmap g) pab

instance Strong p => Strong (Alongside p c d) where
  second' (Alongside pab) = Alongside . dimap shuffle shuffle . second' $ pab
   where
    shuffle (x,(y,z)) = (y,(x,z))

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

-- Could implement this using primitives?
alongside :: Profunctor p =>
          LensLike (Alongside p s' t') s  t  a  b
          -> LensLike (Alongside p a b) s' t' a' b'
          -> LensLike p (s, s') (t, t') (a, a') (b, b')
alongside l1 l2
  = dimap swap swap . getAlongside . l1 . Alongside . dimap swap swap . getAlongside . l2 . Alongside

assoc3L :: Lens ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3L f = assoc3 f

stron :: (Either s s', b) -> Either (s, b) (s', b)
stron (e, b) =  bimap (,b) (, b) e

choosing :: forall s t a b s' t' . Lens s t a b -> Lens s' t' a b -> Lens (Either s s') (Either t t') a b
choosing l r = withLensPrim l (\getl setl ->
                  withLensPrim r (\getr setr ->
                            let --g :: Either s s' -> a
                                g e = case e of
                                        Left v -> let (c, v') = getl v in (Left c, v')
                                        Right v -> let (c, v') = getr v in (Right c, v')
                                s = bimap setl setr . stron
                            in lens g s))

lens :: (s -> (c,a)) -> ((c,b) -> t) -> Lens s t a b
lens get _set = dimap get _set . second'
{-# INLINE lens #-}

------------------------------------------------------------------------------

data ALens a b s t = forall c . ALens (s -> (c,a)) ((c, b) -> t)

instance Functor (ALens a b s) where
  fmap f (ALens _get _set) = ALens _get (f . _set)

instance Profunctor (ALens a b) where
  dimap f g (ALens get _set) = ALens (get . f) (g . _set)

instance Strong (ALens a b) where
  second' (ALens get _set) = ALens get' set' --(bimap id _set . assoc)
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

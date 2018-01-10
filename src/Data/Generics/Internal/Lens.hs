{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

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
import Data.Profunctor        (Choice(..), Profunctor(..))
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Tagged
import GHC.Generics           ((:*:)(..), (:+:)(..), Generic(..), M1(..), K1(..), Rep)
import Data.Coerce

-- | Type alias for lens
type Lens' s a
  = Lens s s a a

type Lens s t a b
  = forall f. Functor f => (a -> f b) -> s -> f t

type LensLike f s t a b
  = (a -> f b) -> s -> f t

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
{-# INLINE first #-}

-- | Lens focusing on the second element of a product
second :: Lens ((a :*: b) x) ((a :*: b') x) (b x) (b' x)
second f (a :*: b)
  = fmap (a :*:) (f b)
{-# INLINE second #-}

left :: Prism ((a :+: c) x) ((b :+: c) x) (a x) (b x)
left = prism L1 $ gsum Right (Left . R1)
{-# INLINE left #-}

right :: Prism ((a :+: b) x) ((a :+: c) x) (b x) (c x)
right = prism R1 $ gsum (Left . L1) Right
{-# INLINE right #-}

gsum :: (a x -> c) -> (b x -> c) -> ((a :+: b) x) -> c
gsum f _ (L1 x) =  f x
gsum _ g (R1 y) =  g y
{-# INLINE gsum #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

-- | A type and its generic representation are isomorphic
repIso :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b x)
repIso = iso from to
{-# INLINE repIso #-}

repLens :: (Generic a, Generic b) => Lens a b (Rep a x) (Rep b x)
repLens f s = repIso f s
{-# INLINE repLens #-}

-- | 'M1' is just a wrapper around `f p`
mIso :: Iso (M1 i c f p) (M1 i c g p) (f p) (g p)
mIso = iso unM1 M1
{-# INLINE mIso #-}

mLens :: Lens (M1 i c f p) (M1 i c g p) (f p) (g p)
mLens f s = mIso f s
{-# INLINE mLens #-}

kIso :: Iso (K1 r a p) (K1 r b p) a b
kIso = iso unK1 K1
{-# INLINE kIso #-}

sumIso :: Iso ((a :+: b) x) ((a' :+: b') x) (Either (a x) (b x)) (Either (a' x) (b' x))
sumIso = iso back forth
  where forth (Left l)  = (L1 l)
        forth (Right r) = (R1 r)
        back (L1 l) = (Left l)
        back (R1 r) = (Right r)
{-# INLINE sumIso #-}

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)

_Right :: Prism (Either c a) (Either c b) a b
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

--------------------------------------------------------------------------------

newtype AlongsideLeft f b a = AlongsideLeft { getAlongsideLeft :: f (a, b) }
newtype AlongsideRight f b a = AlongsideRight { getAlongsideRight :: f (b, a) }

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

instance Functor f => Functor (AlongsideLeft f b) where
  fmap f = AlongsideLeft . fmap (\(a, b) -> (f a, b)) . getAlongsideLeft

instance Functor f => Functor (AlongsideRight f b) where
  fmap f = AlongsideRight . fmap (\(a, b) -> (a, f b)) . getAlongsideRight

alongside :: LensLike (AlongsideLeft f b') s  t  a  b
          -> LensLike (AlongsideRight f t) s' t' a' b'
          -> LensLike f (s, s') (t, t') (a, a') (b, b')
alongside l1 l2 f (a1, a2)
  = getAlongsideRight $ l2 ?? a2 $ \b2 -> AlongsideRight
  $ getAlongsideLeft  $ l1 ?? a1 $ \b1 -> AlongsideLeft
  $ f (b1,b2)
{-# INLINE alongside #-}

prodIso :: Iso ((a :*: b) x) ((a' :*: b') x) (a x, b x) (a' x, b' x)
prodIso = iso (\(a :*: b) -> (a, b)) (\(a, b) -> (a :*: b))
{-# INLINE prodIso #-}

prodL :: Lens ((a :*: b) x) ((a' :*: b') x) (a x, b x) (a' x, b' x)
prodL f s = prodIso f s
{-# INLINE prodL #-}

prodR :: Lens (a' x, b' x) (a x, b x) ((a' :*: b') x) ((a :*: b) x)
prodR f s = fromIso prodIso f s
{-# INLINE prodR #-}

assoc3 :: Iso ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3 = iso (\((a, b), c) -> (a, (b, c))) (\(a, (b, c)) -> ((a, b), c))
{-# INLINE assoc3 #-}

assoc3L :: Lens ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3L f s = assoc3 f s
{-# INLINE assoc3L #-}

assoc3R :: Lens (a', (b', c')) (a, (b, c)) ((a', b'), c') ((a, b), c)
assoc3R f s = fromIso assoc3 f s
{-# INLINE assoc3R #-}

choosing :: Lens s t a b -> Lens s' t' a b -> Lens (Either s s') (Either t t') a b
choosing l _ f (Left a)   = Left <$> l f a
choosing _ r f (Right a') = Right <$> r f a'
{-# INLINE choosing #-}

--------------------------------------------------------------------------------
-- Iso stuff

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE rmap #-}
  ( #. ) _ = coerce
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = coerce p
  {-# INLINE ( .# ) #-}

fromIso :: Iso s t a b -> Iso b a t s
fromIso l = withIso l $ \ sa bt -> iso bt sa
{-# INLINE fromIso #-}

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity #. bt)

pairing :: Iso s t a b -> Iso s' t' a' b' -> Iso (s, s') (t, t') (a, a') (b, b')
pairing f g = withIso f $ \ sa bt -> withIso g $ \s'a' b't' ->
  iso (bmap sa s'a') (bmap bt b't')
  where bmap f' g' (a, b) = (f' a, g' b)

--------------------------------------------------------------------------------
-- Prism stuff

type APrism s t a b = Market a b a (Identity b) -> Market a b s (Identity t)

-- without :: APrism s t a b
--         -> APrism u v c d
--         -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
-- without k =
--   withPrism k         $ \bt seta k' ->
--   withPrism k'        $ \dv uevc    ->
--   prism (bimapE bt dv) $ \su ->
--   case su of
--     Left s  -> bimapE Left Left (seta s)
--     Right u -> bimapE Right Right (uevc u)
--   where bimapE :: (a -> b) -> (c -> d) -> Either a c -> Either b d
--         bimapE f _ (Left a) = Left (f a)
--         bimapE _ g (Right a) = Right (g a)
--         {-# INLINE bimapE #-}

without' :: APrism s t a b -> APrism s t c d -> Prism s t (Either a c) (Either b d)
without' k =
  withPrism k  $ \bt _ k' ->
  withPrism k' $ \dt setc ->
    prism (foldEither bt dt) $ \s -> fmap Right (setc s)
  where foldEither _ g (Right r) = g r
        foldEither f _ (Left l) = f l

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case coerce (k (Market Identity Right)) of
  Market bt seta -> f bt seta

--------------------------------------------------------------------------------
-- Market

data Market a b s t = Market (b -> t) (s -> Either t a)


instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
  lmap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE lmap #-}
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE rmap #-}
  ( #. ) _ = coerce
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = coerce p
  {-# INLINE ( .# ) #-}

instance Choice (Market a b) where
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

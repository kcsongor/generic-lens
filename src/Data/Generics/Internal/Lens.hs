{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Profunctor        (Choice(..), Profunctor(..), Strong(..))
import Data.Bifunctor
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Tagged
import GHC.Generics           ((:*:)(..), (:+:)(..), Generic(..), M1(..), K1(..), Rep)
import Data.Coerce

-- | Type alias for lens
type LensVL' s a
  = Lens s s a a

type LensVL s t a b
  = forall f. Functor f => (a -> f b) -> s -> f t

type Lens s t a b
  = forall p . (Strong p) => p a b -> p s t

-- | Type alias for traversal
type Traversal' s a
  = forall f. Applicative f => (a -> f a) -> s -> f s

type Traversal s t a b
  = forall f. Applicative f => (a -> f b) -> s -> f t

type LensLike p s t a b
  = p a b -> p s t

-- | Type alias for prism
type PrismVL s t a b
  = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism s t a b
  = forall p . (Choice p) => p a b -> p s t

type Prism' s a = forall p . (Choice p) => p a a -> p s s

type PrismVL' s a
  = PrismVL s s a a

type IsoVL' s a
  = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

type IsoVL s t a b
  = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type Iso s t a b
  = forall p. (Profunctor p) => p a b -> p s t

type Iso' s a = Iso s s a a

-- | Getting
(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)
infixl 8 ^.

-- | Setting
set :: ((a -> b) -> s -> t) -> (s, b) -> t
set f (s, b)
  = f  (const b) s

viewVL :: ((a -> Const a a) -> s -> Const a s) -> s -> a
viewVL l s = (^.) s l

viewVL' :: ((a -> Const a a) -> s -> Const a s) -> s -> a
viewVL' l s = (^.) s l

setVL :: LensVL s t a b -> b -> s -> t
setVL l x = runIdentity . (l (Identity . const x))


view :: Lens s s a a -> s -> a
view l = withLens l (\get _ -> get)

withLens :: Lens s t a b -> ((s -> a) -> ((s, b) -> t) -> r) -> r
withLens l k =
 case l idLens of
   ALens get set -> k get set

idLens :: ALens a b a b
idLens = ALens id snd

infixr 4 .~
(.~) :: ((a -> b) -> s -> t) -> b -> s -> t
(.~) f b s = set f (s, b)

infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)

buildVL :: (Tagged b (Identity b) -> Tagged t (Identity t)) -> b -> t
buildVL p = runIdentity #. unTagged #. p .# Tagged .# Identity

matchVL :: PrismVL s t a b -> s -> Either t a
matchVL k = withPrismVL k $ \_ match -> match



build :: (Tagged b b -> Tagged t t) -> b -> t
build p = unTagged . p . Tagged


-- | Lens focusing on the first element of a product
first :: Lens ((a :*: b) x) ((a' :*: b) x) (a x) (a' x)
first
  = lens (\(a :*: b) -> a) (\((a :*: b), a') -> a' :*: b)

-- | Lens focusing on the second element of a product
second :: Lens ((a :*: b) x) ((a :*: b') x) (b x) (b' x)
second
  = lens (\(a :*: b) -> b) (\((a :*: b), b') -> a :*: b')

left :: Prism ((a :+: c) x) ((b :+: c) x) (a x) (b x)
left = prism L1 $ gsum Right (Left . R1)

right :: Prism ((a :+: b) x) ((a :+: c) x) (b x) (c x)
right = prism R1 $ gsum (Left . L1) Right

gsum :: (a x -> c) -> (b x -> c) -> ((a :+: b) x) -> c
gsum f _ (L1 x) =  f x
gsum _ g (R1 y) =  g y

prismVL :: (b -> t) -> (s -> Either t a) -> PrismVL s t a b
prismVL bt seta eta = dimap (\x -> plus pure id (seta x)) (either id (\x -> fmap bt x)) (right' eta)
{-# INLINE prismVL #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta eta = dimap seta (either id bt) (right' eta)

plus :: (a -> b) -> (c -> d) -> Either a c -> Either b d
plus f _ (Left x) = Left (f x)
plus _ g (Right y) = Right (g y)

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g a = (f a, g a)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross = bimap

-- | A type and its generic representation are isomorphic
repIsoVL :: (Generic a, Generic b) => IsoVL a b (Rep a x) (Rep b x)
repIsoVL = isoVL from to

repLens :: (Generic a, Generic b) => Lens a b (Rep a x) (Rep b x)
repLens f = repIso f

-- | A type and its generic representation are isomorphic
repIso :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b x)
repIso = iso from to



-- | 'M1' is just a wrapper around `f p`
mIsoVL :: IsoVL (M1 i c f p) (M1 i c g p) (f p) (g p)
mIsoVL = isoVL unM1 M1

-- | 'M1' is just a wrapper around `f p`
--mIso :: Iso' (M1 i c f p) (f p)
mIso :: Iso (M1 i c f p) (M1 i c g p) (f p) (g p)
mIso = iso unM1 M1

-- These are specialised versions of the Isos above. On GHC 8.0.2, having
-- these functions eta-expanded allows the optimiser to inline these functions.
mLens :: Lens (M1 i c f p) (M1 i c g p) (f p) (g p)
mLens f = mIso f

kIso :: Iso (K1 r a p) (K1 r b p) a b
kIso = iso unK1 K1

sumIso :: Iso ((a :+: b) x) ((a' :+: b') x) (Either (a x) (b x)) (Either (a' x) (b' x))
sumIso = iso back forth
  where forth (Left l)  = (L1 l)
        forth (Right r) = (R1 r)
        back (L1 l) = (Left l)
        back (R1 r) = (Right r)

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

lens2lensvl :: ALens a b s t -> LensVL s t a b
lens2lensvl (ALens get set) = lensVL get set

ravel :: (ALens a b a b -> ALens a b s t)
      ->  LensVL s t a b
ravel l pab = (lens2lensvl $ l idLens) pab

prismPRavel ::
                 (Market a b a b -> Market a b s t)
                 -> Prism s t a b
prismPRavel l pab = (prism2prismp $ l idPrism) pab

prismVLRavel ::
                 (Market a b a b -> Market a b s t)
                 -> PrismVL s t a b
prismVLRavel l pab  = (prism2prismvl $ l idPrism) pab

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

prodIsoVL :: IsoVL ((a :*: b) x) ((a' :*: b') x) (a x, b x) (a' x, b' x)
prodIsoVL = isoVL (\(a :*: b) -> (a, b)) (\(a, b) -> (a :*: b))

prodIso :: Iso ((a :*: b) x) ((a' :*: b') x) (a x, b x) (a' x, b' x)
prodIso = iso (\(a :*: b) -> (a, b)) (\(a, b) -> (a :*: b))

prodL :: Lens ((a :*: b) x) ((a' :*: b') x) (a x, b x) (a' x, b' x)
prodL f = prodIso f

prodR :: Lens (a' x, b' x) (a x, b x) ((a' :*: b') x) ((a :*: b) x)
prodR f = fromIso prodIso f

assoc3 :: Iso ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3 = iso (\((a, b), c) -> (a, (b, c))) (\(a, (b, c)) -> ((a, b), c))

assoc3L :: Lens ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3L f = assoc3 f

assoc3R :: Lens (a', (b', c')) (a, (b, c)) ((a', b'), c') ((a, b), c)
assoc3R f = fromIso assoc3 f

stron :: (Either s s', b) -> Either (s, b) (s', b)
stron (e, b) =  bimap (,b) (, b) e

choosing :: forall s t a b s' t' . Lens s t a b -> Lens s' t' a b -> Lens (Either s s') (Either t t') a b
choosing l r = withLens l (\getl setl ->
                  withLens r (\getr setr ->
                            let g :: Either s s' -> a
                                g = either getl getr
                                s = bimap setl setr . stron
                            in lens g s))

lens :: (s -> a) -> ((s,b) -> t) -> Lens s t a b
lens get set = dimap (fork id get) set . second'

lensVL :: (s -> a) -> ((s, b) -> t) -> LensVL s t a b
lensVL get set f x = curry set x <$> f (get x)

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

isoVL :: (s -> a) -> (b -> t) -> IsoVL s t a b
isoVL sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap

withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id id) of
  Exchange sa bt -> k sa bt

pairing :: Iso s t a b -> Iso s' t' a' b' -> Iso (s, s') (t, t') (a, a') (b, b')
pairing f g = withIso f $ \ sa bt -> withIso g $ \s'a' b't' ->
  iso (bmap sa s'a') (bmap bt b't')
  where bmap f' g' (a, b) = (f' a, g' b)

--------------------------------------------------------------------------------
-- Prism stuff

type APrism s t a b = Market a b a b -> Market a b s t
type APrismVL s t a b = Market a b a (Identity b) -> Market a b s (Identity t)

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

without' :: Prism s t a b -> Prism s t c d -> Prism s t (Either a c) (Either b d)
without' k =
  withPrism k  $ \bt _ k' ->
  withPrism k' $ \dt setc ->
    prism (foldEither bt dt) $ \s -> fmap Right (setc s)
  where foldEither _ g (Right r) = g r
        foldEither f _ (Left l) = f l

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case k idPrism of
  Market bt seta -> f bt seta

withPrismVL :: APrismVL s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrismVL k f = case coerce (k (Market Identity Right)) of
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

prism2prismp :: Market a b s t -> Prism s t a b
prism2prismp (Market bt seta) = prism bt seta

prism2prismvl :: Market a b s t -> PrismVL s t a b
prism2prismvl  (Market bt seta) = prismVL bt seta
{-# INLINE prism2prismvl #-}

idPrism :: Market a b a b
idPrism = Market id Right

------------------------------------------------------------------------------

data ALens a b s t = ALens (s -> a) ((s, b) -> t)

instance Functor (ALens a b s) where
  fmap f (ALens get set) = ALens get (f . set)

instance Profunctor (ALens a b) where
  dimap f g (ALens get set) = ALens (get . f) (g . set . cross f id)

instance Strong (ALens a b) where
  second' (ALens get set) = ALens (get . snd) (bimap id set . assoc)
    where
      assoc ((a, b), c) = (a, (b, c))

newtype Curried f a =
  Curried { runCurried :: forall r. f (a -> r) -> f r }

instance Functor f => Functor (Curried f) where
  fmap f (Curried g) = Curried (g . fmap (.f))
  {-# INLINE fmap #-}

instance (Functor f) => Applicative (Curried f) where
  pure a = Curried (fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}

-- | The natural isomorphism between @f@ and @Curried f f@.
-- @
-- 'lowerCurried' '.' 'liftCurried' ≡ 'id'
-- 'liftCurried' '.' 'lowerCurried' ≡ 'id'
-- @
--
-- @
-- 'lowerCurried' ('liftCurried' x)     -- definition
-- 'lowerCurried' ('Curried' ('<*>' x))   -- definition
-- ('<*>' x) ('pure' 'id')          -- beta reduction
-- 'pure' 'id' '<*>' x              -- Applicative identity law
-- x
-- @
liftCurried :: Applicative f => f a -> Curried f a
liftCurried fa = Curried (<*> fa)

-- | Lower 'Curried' by applying 'pure' 'id' to the continuation.
--
-- See 'liftCurried'.
lowerCurried :: Applicative f => Curried f a -> f a
lowerCurried (Curried f) = f (pure id)

confusing :: Applicative f => Traversal s t a b -> (a -> f b) -> s -> f t
confusing t = \f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
  where
  liftCurriedYoneda :: Applicative f => f a -> Curried (Yoneda f) a
  liftCurriedYoneda fa = Curried (`yap` fa)
  {-# INLINE liftCurriedYoneda #-}

  yap :: Applicative f => Yoneda f (a -> b) -> f a -> Yoneda f b
  yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa)
  {-# INLINE yap #-}
{-# INLINE confusing #-}

-- | @Yoneda f a@ can be viewed as the partial application of 'fmap' to its second argument.
newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

-- | The natural isomorphism between @f@ and @'Yoneda' f@ given by the Yoneda lemma
-- is witnessed by 'liftYoneda' and 'lowerYoneda'
--
-- @
-- 'liftYoneda' . 'lowerYoneda' ≡ 'id'
-- 'lowerYoneda' . 'liftYoneda' ≡ 'id'
-- @
--
-- @
-- lowerYoneda (liftYoneda fa) =         -- definition
-- lowerYoneda (Yoneda (\f -> fmap f a)) -- definition
-- (\f -> fmap f fa) id                  -- beta reduction
-- fmap id fa                            -- functor law
-- fa
-- @
--
-- @
-- 'lift' = 'liftYoneda'
-- @
liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda a = Yoneda (\f -> fmap f a)

lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda f) = f id

instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)

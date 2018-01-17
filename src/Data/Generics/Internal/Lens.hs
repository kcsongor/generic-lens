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
import GHC.Generics           ((:*:)(..), (:+:)(..), Generic(..), M1(..), Rep)

-- | Type alias for lens
type Lens' s a
  = Lens s s a a

type Lens s t a b
  = forall f. Functor f => (a -> f b) -> s -> f t

-- | Type alias for traversal
type Traversal' s a
  = forall f. Applicative f => (a -> f a) -> s -> f s

type Traversal s t a b
  = forall f. Applicative f => (a -> f b) -> s -> f t

-- | Type alias for prism
type Prism s t a b
  = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type PrismP s t a b
  = forall p . (Choice p) => p a b -> p s t

type Prism' s a
  = Prism s s a a

type Iso' s a
  = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

type Iso s t a b
  = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type IsoP s t a b
  = forall p. (Profunctor p) => p a b -> p s t

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

leftP :: PrismP ((a :+: c) x) ((b :+: c) x) (a x) (b x)
leftP = prismP L1 $ gsum Right (Left . R1)

right :: Prism ((a :+: b) x) ((a :+: c) x) (b x) (c x)
right = prism R1 $ gsum (Left . L1) Right

rightP :: PrismP ((a :+: b) x) ((a :+: c) x) (b x) (c x)
rightP = prismP R1 $ gsum (Left . L1) Right

gsum :: (a x -> c) -> (b x -> c) -> ((a :+: b) x) -> c
gsum f _ (L1 x) =  f x
gsum _ g (R1 y) =  g y

combine :: Lens' (s x) a -> Lens' (t x) a -> Lens' ((s :+: t) x) a
combine sa _ f (L1 s) = fmap (\a -> L1 (set sa a s)) (f (s ^. sa))
combine _ ta f (R1 t) = fmap (\a -> R1 (set ta a t)) (f (t ^. ta))

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta eta = dimap (\x -> plus pure id (seta x)) (either id (\x -> fmap bt x)) (right' eta)
{-# INLINE prism #-}

prismP :: (b -> t) -> (s -> Either t a) -> PrismP s t a b
prismP bt seta eta = dimap seta (either id bt) (right' eta)
{-# INLINE prismP #-}

plus :: (a -> b) -> (c -> d) -> Either a c -> Either b d
plus f _ (Left x) = Left (f x)
plus _ g (Right y) = Right (g y)


-- | A type and its generic representation are isomorphic
repIso :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b x)
repIso = dimap from (fmap to)

-- | A type and its generic representation are isomorphic
repIsoP :: (Generic a, Generic b) => IsoP a b (Rep a x) (Rep b x)
repIsoP = dimap from to


-- | 'M1' is just a wrapper around `f p`
--mIso :: Iso' (M1 i c f p) (f p)
mIso :: Iso (M1 i c f p) (M1 i c g p) (f p) (g p)
mIso = dimap unM1 (fmap M1)

-- | 'M1' is just a wrapper around `f p`
--mIso :: Iso' (M1 i c f p) (f p)
mIsoP :: IsoP (M1 i c f p) (M1 i c g p) (f p) (g p)
mIsoP = dimap unM1 M1

-- These are specialised versions of the Isos above. On GHC 8.0.2, having
-- these functions eta-expanded allows the optimiser to inline these functions.
mLens :: Lens (M1 i c f p) (M1 i c g p) (f p) (g p)
mLens f s = mIso f s

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

--------------------------------------------------------------------------------

-- Pull dimaps to left and rights to right

{-
instance Choice p => Profunctor (MergeRight p) where
  dimap f g (MergeRight (Dirty pab)) = MergeRight (DMap f g pab)
  dimap f g (MergeRight (DMap f' g' pab)) = MergeRight (DMap (f' . f) (g . g') pab)
  dimap f g (MergeRight (DRight pab))     =  MergeRight (DMap f g (right' pab))

instance Choice p => Choice (MergeRight p) where
  right' (MergeRight (Dirty pab)) = MergeRight (DRight pab)
  right' (MergeRight (DMap f g pab)) = MergeRight (DMap (fmap f) (fmap g) (right' pab))
  right' (MergeRight (DRight pab))   = MergeRight (DMap assoc' assoc (right' pab))
  -}
{-
data PrismBoggle p a b where
  DRight :: (p x y) -> PrismBoggle p (Either z x) (Either z y)
  DMap :: (c -> a) -> (b -> d) -> (p a b) -> PrismBoggle p c d
  Dirty :: p a b -> PrismBoggle p a b

instance Choice p => Profunctor (PrismBoggle p) where
  dimap f g (Dirty pab) = DMap f g pab
  dimap f g (DMap f' g' pab) = DMap (f' . f) (g . g') pab
  dimap f g (DRight pab)  =  DMap f g (right' pab)

instance Choice p => Choice (PrismBoggle p) where
  right' (Dirty pab) = DRight pab
  right' (DMap f g pab) = DMap (fmap f) (fmap g) (right' pab)
  right' (DRight pab)   = DMap assoc assoc' (right' pab)

assoc :: Either a (Either b c) -> Either (Either a b) c
assoc e = case e of
            Left a -> Left (Left a)
            Right ebc -> case ebc of
                           Left b -> Left (Right b)
                           Right c -> Right c

assoc' :: Either (Either a b) c -> Either a (Either b c)
assoc' e = case e of
             Right c -> Right (Right c)
             Left eab -> case eab of
                           Left a -> Left a
                           Right b -> Right (Left b)

projPrism :: p a b -> PrismBoggle p a b
projPrism pab = Dirty pab

--rightFuse :: Choice p => p a b -> p (Either c (Either d a)) (Either c (Either d b))
rightFuse x = injPrism (injPrism (right' (right' (right' (projPrism (projPrism x))))))


injPrism :: Choice p => PrismBoggle p a b -> p a b
injPrism (Dirty pab) = pab
injPrism (DMap f g pab) = dimap f g pab
injPrism (DRight pab) = right' pab
-}
-- We don't need the full power of boggle, we just want to fuse together
-- two `fmaps`
{-
prismRavel ::
                 (forall p . (MProfunctor p, MRight p) =>  p a b -> p s t)
                 -> Prism s t a b
prismRavel l pab = wrappedProfunctor (lowerFusionStack (ddimap id lowerBoggle (wrappedVLMProfunctor (ravelFusionStack l (WrappedVLMProfunctor (ddimap id (liftBoggle) (liftFusionStack (WrappedProfunctor pab))))))))
-}

prismPRavel ::
                 (Market a b a b -> Market a b s t)
                 -> PrismP s t a b
prismPRavel l pab = (prism2prismp $ l idPrism) pab
{-# INLINE prismPRavel #-}

prismRavel ::
                 (Market a b a b -> Market a b s t)
                 -> Prism s t a b
prismRavel l pab  = (prism2prismvl $ l idPrism) pab
{-# INLINE prismRavel #-}

------------------------------------------------------------------------------
-- Prism: Market
------------------------------------------------------------------------------

-- | This type is used internally by the 'Control.Lens.Prism.Prism' code to
-- provide efficient access to the two parts of a 'Prism'.
data Market a b s t = Market (b -> t) (s -> Either t a)

-- | @type 'Market'' a s t = 'Market' a a s t@
type Market' a = Market a a

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}

instance Choice (Market a b) where
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE right' #-}

prism2prismp :: Market a b s t -> PrismP s t a b
prism2prismp (Market bt seta) = prismP bt seta

prism2prismvl :: Market a b s t -> Prism s t a b
prism2prismvl  (Market bt seta) = prism bt seta
{-# INLINE prism2prismvl #-}

idPrism :: Market a b a b
idPrism = Market id Right

------------------------------------------------------------------------------

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
{-# INLINE liftCurried #-}

-- | Lower 'Curried' by applying 'pure' 'id' to the continuation.
--
-- See 'liftCurried'.
lowerCurried :: Applicative f => Curried f a -> f a
lowerCurried (Curried f) = f (pure id)
{-# INLINE lowerCurried #-}

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




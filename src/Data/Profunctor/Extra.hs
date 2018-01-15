{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funfolding-creation-threshold=100000 #-}
module Data.Profunctor.Extra where

import Data.Profunctor
import GHC.Generics
import Debug.Trace
import Boggle hiding ((<.>))


-- Pull dimaps to left and rights to right
{-
data Boggle p a b where
  DRight :: (p x y) -> Boggle p (Either z x) (Either z y)
  DMap :: (c -> a) -> (b -> d) -> (p a b) -> Boggle p c d
  Dirty :: p a b -> Boggle p a b

instance Choice p => Profunctor (Boggle p) where
  dimap f g (Dirty pab) = DMap f g pab
  dimap f g (DMap f' g' pab) = DMap (f' . f) (g . g') pab
  dimap f g (DRight pab)     =  DMap f g (right' pab)

instance Choice p => Choice (Boggle p) where
  right' (Dirty pab) = DRight pab
  right' (DMap f g pab) = DMap (fmap f) (fmap g) (right' pab)
  right' (DRight pab)   = DRight (right' pab)

lift :: p a b -> Boggle p a b
lift pab = Dirty pab


lower :: Choice p => Boggle p a b -> p a b
lower (Dirty pab) = pab
lower (DMap f g pab) = dimap f g pab
lower (DRight pab) = right' pab

test1 :: Choice p => p Int Int -> p (Either z Int) (Either z Int)
test1 pab = right' (dimap (+1) (subtract 1) pab)

test1spec :: Choice p => p Int Int -> p (Either z Int) (Either z Int)
test1spec = lower . test1 . lift
-}


------------------------------------------------------------------------------

--- This type fuses together uses of `dimap` we want to do this last
-- as we first need to rearrange the "Eithers"
newtype DimapK p b c = DimapK (forall a d . (Fun a b -> Fun c d -> p a d))

liftDimapK :: MProfunctor p => p a b -> DimapK p a b
liftDimapK pa = DimapK (\f g -> mdimap f g pa)

lowerDimapK :: DimapK p a b -> p a b
lowerDimapK (DimapK pa) = pa (liftFun id) (liftFun id)

-- Note that here because we use (<.>) we potentially flatten "either"
instance MProfunctor (DimapK p) where
  mdimap f g (DimapK x) = DimapK (\y z -> x (f <.> y) (z <.> g))
  {-# INLINE mdimap #-}

fmdimap :: Fun a b -> Fun c d -> DimapK p b c -> p a d
fmdimap f g (DimapK x) = x f g

------------------------------------------------------------------------------
-- Wrapper to ensure that a liftDimapK followed by lowerDimapK doesn't
-- introduce a dimap.

data DimapK1 p b c = DimapK1 (p b c) (DimapK p b c)
                   | DirtyDimapK1 (p b c)

liftDimapK1 :: MProfunctor p => p a b -> DimapK1 p a b
liftDimapK1 pab = DimapK1 pab (liftDimapK pab)

lowerDimapK1 :: DimapK1 p a b -> p a b
lowerDimapK1 (DimapK1 pab _) = pab
lowerDimapK1 (DirtyDimapK1 pab) = pab

instance MProfunctor p => MProfunctor (DimapK1 p) where
  mdimap f g (DimapK1 _ d) = DimapK1 (fmdimap f g d) (mdimap f g d)
  mdimap f g (DirtyDimapK1 pab) = DimapK1 (mdimap f g pab) (mdimap f g (liftDimapK pab))
  {-# INLINE mdimap #-}

--instance MRight p => MRight (DiMapK1 p) where
--  mright (DimapK1
instance MRight p => MRight (DimapK1 p) where
  mright (DimapK1 f g) = DirtyDimapK1 (mright f)
  mright (DirtyDimapK1 pab) = DirtyDimapK1 (mright pab)
  {-# INLINE mright #-}

-- Should be the identity, works
testDimapK1 :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
testDimapK1 pab = lowerDimapK1 (liftDimapK1 pab)

a, b :: Fun Int Int
a = liftFun id
b = liftFun id

-- Should observe one dimap in the specialised version
testDimapK1A :: (MProfunctor p) => p Int Int -> p Int Int
testDimapK1A pab = mdimap a b (mdimap a b pab)

ravelDimapK1 :: (MProfunctor p)
             => (DimapK1 p a b -> DimapK1 p c d)
             -> p a b -> p c d
ravelDimapK1 l pab = lowerDimapK1 (l (liftDimapK1 pab))

-- As expected
specDimapK1A :: (MProfunctor p) => p Int Int -> p Int Int
specDimapK1A = ravelDimapK1 testDimapK1A




-------------------------------------------------------------------------------
-- Pull Dimaps to the left

data DimapSyntax p c d where
  DMSMap :: Fun c a -> Fun b d -> p a b -> DimapSyntax p c d
  DMSDirty :: p a b -> DimapSyntax p a b

liftDimapSyntax :: p a b -> DimapSyntax p a b
liftDimapSyntax pab = DMSDirty pab

lowerDimapSyntax :: MProfunctor p => DimapSyntax p a b -> p a b
lowerDimapSyntax (DMSDirty pab) = pab
lowerDimapSyntax (DMSMap pca pbd pab) = mdimap pca pbd pab

instance MRight p => MRight (DimapSyntax p) where
  mright (DMSDirty pab) = DMSDirty (mright pab)
  mright (DMSMap fca fbd pab) = DMSMap (femap fca) (femap fbd) (mright pab)
  {-# INLINE mright #-}

-- Don't fuse here, we do that in DimapK1 at the end
instance MProfunctor p => MProfunctor (DimapSyntax p) where
  mdimap f g (DMSDirty p) = DMSMap f g p
  mdimap f g (DMSMap f' g' p) = DMSMap (f' <.> f) (g <.> g') p
  {-# INLINE mdimap #-}

f :: Fun Int (Either Int Int)
f = undefined
g :: Fun (Either Int Int) Int
g = undefined


testDimapSyntax :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
testDimapSyntax pab = mdimap f g (mright (mdimap f g (mright pab)))

ravelDimapSyntax :: (MProfunctor p, MRight p) =>
                 (DimapSyntax p a b -> DimapSyntax p s t)
                 -> p a b -> p s t
ravelDimapSyntax l pab = lowerDimapSyntax (l (liftDimapSyntax pab))

ravelDimapSyntaxFF :: (MProfunctor p, MRight p) =>
                 (DimapSyntax (RightFuse (RightFreeTheorem p)) a b -> DimapSyntax (RightFuse (RightFreeTheorem p)) s t)
                 -> p a b -> p s t
ravelDimapSyntaxFF l pab = lowerRightFreeTheorem (lowerRightFuse (lowerDimapSyntax (l (liftDimapSyntax (liftRightFuse (liftRightFreeTheorem pab))))))

-- Works
specDimapSyntax :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
specDimapSyntax = ravelDimapSyntax testDimapSyntax


-------------------------------------------------------------------------------
-- Fuse together rights
{-
data RightSyntax p c d where
  RSRight :: Fun c a -> Fun b d -> p a b -> DimapSyntax p c d
  RSDirty :: p a b -> DimapSyntax p a b

liftRightSyntax :: p a b -> RightSyntax p a b
liftRightSyntax pab = RSDirty pab

lowerRightSyntax :: MRight p => RightSyntax p a b -> p a b
lowerRightSyntax (RSDirty pab) = pab
lowerRightSyntax (RSRight paa) = mright pa

instance MRight p => MRight (DimapSyntax p) where
  mright (DMSDirty pab) = DMSDirty (mright pab)
  mright (DMSMap fca fbd pab) = DMSMap (ffmap fca) (ffmap fbd) (mright pab)

-- Don't fuse here, we do that in DimapK1 at the end
instance MProfunctor p => MProfunctor (DimapSyntax p) where
  mdimap f g (DMSDirty p) = DMSMap f g p
  mdimap f g (DMSMap f' g' p) = DMSMap f g (mdimap f' g' p)
  -}
-- We always want to ensure that "RightFuse p b c ~ p b c" So after
-- applying the "RightFuse" constructor when we encounter a right, we need
-- to ensure the type is "p (Either z b) (Either z c) as that's the type of
-- applying right
data RightFuse p b c where
  RightFuse :: (forall a d z . Fun a (Either z b) -> Fun (Either z c) d -> p a d) -> RightFuse p (Either z b) (Either z c)
  DirtyRightFuse :: p b c -> RightFuse p b c

initRightFuse :: (MRight p, MProfunctor p) => p a b -> RightFuse p (Either z a) (Either z b)
initRightFuse pa = RightFuse (\f g -> mdimap f g (mright pa))

liftRightFuse :: p a b -> RightFuse p a b
liftRightFuse pa = DirtyRightFuse pa

lowerRightFuse :: RightFuse p a b -> p a b
lowerRightFuse (DirtyRightFuse pa) = pa
lowerRightFuse (RightFuse pa) = pa (liftFun id) (liftFun id)

instance (MProfunctor p, MRight p) => MRight (RightFuse p) where
  -- Both these work but this version maintains the EitherFun invariant
  mright (RightFuse r) = RightFuse (\f1 f2 -> r (assocNew <.> f1)  (f2 <.> assoc'New))
--  mright (RightFuse r) = RightFuse (\f1 f2 -> r (assoc <.> f1)  (f2 <.> assoc'))
  mright (DirtyRightFuse pab) = initRightFuse pab
  {-# INLINE mright #-}

-- Once we encounter a dimap we're done. As expressions are already in
-- a normal form.            | We are here
--                           |
-- dimap . dimap . dimap . <-| right . right . right
instance MProfunctor p => MProfunctor (RightFuse p) where
  mdimap f g (RightFuse p) =  DirtyRightFuse (p f g)
--    RightFuse (\aezb ezcd -> p (EitherFun Left (Right . lowerFun f) <.> aezb)
--                               (ezcd <.> EitherFun Left (Right . lowerFun g)))

  -- We apply this adapther after floating all dimaps to the left so that
  -- this should never happen
  mdimap f g (DirtyRightFuse p) = DirtyRightFuse (mdimap f g p)
  {-# INLINE mdimap #-}

-- These are assoc and assoc' but with the free theorem applied
assocNew = WrappedFun (plus assocg id) <.> assoc
assoc'New = EitherFun id (\c -> Right (Right c))

assocg = (\eab -> case eab of
      Left a -> Left a
      Right b -> Right (Left b))

assoc :: Fun (Either a (Either b c)) (Either (Either a b) c)
assoc = EitherFun (\a -> Left (Left a))
                  (\ebc ->
                      case ebc of
                           Left b -> Left (Right b)
                           Right c -> Right c)

assoc' :: Fun (Either (Either a b) c) (Either a (Either b c))
assoc' = EitherFun
                   (\eab -> case eab of
                           Left a -> Left a
                           Right b -> Right (Left b))
                   (\c -> Right (Right c)    )

-- Want to only have one mright
testRightFuse :: (MProfunctor p, MRight p) => p Int Int -> p (Either z2 (Either z (Either z1 Int))) (Either z2 ((Either z (Either z1 Int))))
testRightFuse pab = mright (mright (mright pab))

ravelRightFuse :: (MProfunctor p, MRight p) =>
                 (RightFuse p a b -> RightFuse p s t)
                 -> p a b -> p s t
ravelRightFuse l pab = lowerRightFuse (l (liftRightFuse pab))

-- Works
specRightFuse :: (MProfunctor p, MRight p) => p Int Int -> p (Either z2 (Either z (Either z1 Int))) (Either z2 ((Either z (Either z1 Int))))
specRightFuse = ravelRightFuse testRightFuse

------------------------------------------------------------------------------
-- Then only start after a right
-- Don't think this is needed actually
{-

data RightFuse1 p b c where
  RightFuse1 :: p (Either z b) (Either z c) -> (RightFuse p (Either z b) (Either z c)) -> RightFuse1 p (Either z b) (Either z c)
  DirtyRightFuse1 :: (p b c) -> RightFuse1 p b c

initRightFuse1 :: (MRight p, MProfunctor p) => p a b -> RightFuse1 p (Either z a) (Either z b)
initRightFuse1 pab = RightFuse1 (mright pab) (initRightFuse pab)

liftRightFuse1 = DirtyRightFuse1

lowerRightFuse1 :: RightFuse1 p a b -> p a b
lowerRightFuse1 (RightFuse1 pab _) = pab
lowerRightFuse1 (DirtyRightFuse1 pbc) = pbc

rfdimap :: Fun c a -> Fun b d -> RightFuse p a b -> p c d
rfdimap f g (RightFuse rf) = rf f g

-- Once we see a dimap, it's over, remember the normal form invariant
instance MProfunctor p => MProfunctor (RightFuse1 p) where
  mdimap f g (RightFuse1 _ d) = DirtyRightFuse1 (rfdimap f g d) -- RightFuse1 (rfdimap f g d) (mdimap f g d)
  mdimap f g (DirtyRightFuse1 pbc) = DirtyRightFuse1 (mdimap f g pbc)

instance (MProfunctor p, MRight p) => MRight (RightFuse1 p) where
  mright (DirtyRightFuse1 pbc) = initRightFuse1 pbc
  mright (RightFuse1 _ d) = RightFuse1 (mright (lowerRightFuse d)) (mright d)

-- Want to only have one mright
testRightFuse1 :: (MProfunctor p, MRight p) => p Int Int -> p (Either z (Either z1 Int)) ((Either z (Either z1 Int)))
testRightFuse1 pab = mright (mright pab)

ravelRightFuse1 :: (MProfunctor p, MRight p) =>
                 (RightFuse1 p a b -> RightFuse1 p s t)
                 -> p a b -> p s t
ravelRightFuse1 l pab = lowerRightFuse1 (l (liftRightFuse1 pab))

-- Fails
specRightFuse1 :: (MProfunctor p, MRight p) => p Int Int -> p (Either z (Either z1 Int)) ((Either z (Either z1 Int)))
specRightFuse1 = ravelRightFuse1 testRightFuse1
-}
------------------------------------------------------------------------------
-- Now apply a specialised version of the free theorem, do this before
-- fusing anything or we risk losing information
-- dimap f (either g h) (right p) = dimap (either g f) (either id h) (right p)

data RightFreeTheorem p a b where
  RFRight :: p a b -> RightFreeTheorem p (Either z a) (Either z b)
--  RFDMap :: Fun c a -> Fun b d -> p a b -> RightFreeTheorem p c d
  RFDirty :: p a b -> RightFreeTheorem p a b

liftRightFreeTheorem :: p a b -> RightFreeTheorem p a b
liftRightFreeTheorem = RFDirty

lowerRightFreeTheorem :: (MProfunctor p, MRight p) => RightFreeTheorem p a b -> p a b
lowerRightFreeTheorem (RFDirty pab) = pab
--lowerRightFreeTheorem (RFDMap fa fb pab) = mdimap fa fb pab
lowerRightFreeTheorem (RFRight pab) = mright pab

instance (MRight p, MProfunctor p) => MProfunctor (RightFreeTheorem p) where
  -- We are interested in dimap ... (right so dimap after a dirty is
  -- pointless so just delegate
  mdimap f g (RFDirty pab) = RFDirty (mdimap f g pab)
  -- Actually I we need to do fusion in the same pass otherwise we just do
  -- this for dimaps immediately next to a right BUT we do a partial fusion
  -- pass first which spots these opporunities
  mdimap f (EitherFun g h) (RFRight pab) = RFDirty (mdimap (liftFun (plus g id) <.> f) (EitherFun id h) (mright pab))
  mdimap f g  (RFRight pab) = RFDirty (mdimap f g (mright pab))
  {-# INLINE mdimap #-}

instance MRight p => MRight (RightFreeTheorem p) where
  mright (RFDirty pab) = RFRight pab
  mright (RFRight pab) = RFRight (mright pab)
  {-# INLINE mright #-}


plus :: (a -> b) -> (c -> d) -> Either a c -> Either b d
plus f g (Left x) = Left (f x)
plus f g (Right x) = Right (g x)


-- Want to only have one mright
testRightFreeTheorem :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
testRightFreeTheorem pab = mdimap f g (mright (mdimap f g (mright pab)))
  where
    f :: Fun Int (Either Int Int)
    f = liftFun Left

    g :: Fun (Either Int Int) Int
    g = EitherFun (const 0) (const 0)

ravelRightFreeTheorem :: (MProfunctor p, MRight p) =>
                 (RightFreeTheorem p a b -> RightFreeTheorem p s t)
                 -> p a b -> p s t
ravelRightFreeTheorem l pab = lowerRightFreeTheorem (l (liftRightFreeTheorem pab))

-- Works
specRightFreeTheorem :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
specRightFreeTheorem = ravelRightFreeTheorem testRightFreeTheorem

------------------------------------------------------------------------------
-- A partial dimap fusion pass which only fuses functions such that
-- we still have an EitherFun in the 2nd argument of dimap.
{-
data PartialFusion p a b where
  PFDMap :: Fun c a -> Fun b d -> p a b -> PartialFusion p c d
  PFDirty :: p a b -> PartialFusion p a b

liftPartialFusion :: p a b -> PartialFusion p a b
liftPartialFusion = PFDirty

lowerPartialFusion :: MProfunctor p => PartialFusion p a b -> p a b
lowerPartialFusion (PFDirty pab) = pab
lowerPartialFusion (PFDMap fca fbd pcd) = mdimap fca fbd pcd

instance (MRight p, MProfunctor p) => MProfunctor (PartialFusion p) where
  mdimap f g (PFDirty pab) = PFDMap f g pab
  mdimap f g (PFDMap f' (EitherFun l r) pab) = PFDMap (f' <.> f) (EitherFun ((lowerFun g) . l) ((lowerFun g) . r)) pab
  mdimap f g (PFDMap f' g' pab) = PFDMap f g (mdimap f' g' pab)

instance (MProfunctor p, MRight p) => MRight (PartialFusion p) where
  mright pab = PFDirty (mright (lowerPartialFusion pab))


-- We want g and g' to fuse but not the innermost dimap so we are left with
-- two dimaps
testPartialFusion :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
testPartialFusion pab = (mdimap id' g (mdimap (WrappedFun Left) g' (mdimap id' id' (mright pab))))
  where
    id' = WrappedFun id

    g' :: Fun (Either Int Int) Int
    g' = EitherFun (const 0) (const 0)

    g :: Fun Int Int
    g = WrappedFun (const 2)


ravelPartialFusion :: (MProfunctor p, MRight p) =>
                 (PartialFusion p a b -> PartialFusion p s t)
                 -> p a b -> p s t
ravelPartialFusion l pab = lowerPartialFusion (l (liftPartialFusion pab))

-- Works
specPartialFusion :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
specPartialFusion = ravelPartialFusion testPartialFusion
-}
------------------------------------------------------------------------------
-- We also lift the function space so that we can rewrite
-- dimap f (either g h) (right r) = dimap (either g f) (either id h) (right r)
--
data Fun a b where
  WrappedFun :: (a -> b) -> Fun a b
--  PlusFun :: (x1 -> x2) -> (y1 -> y2) -> Fun (Either x1 y1) (Either x2 y2)
  EitherFun :: (x1 -> z) -> (x2 -> z) -> Fun (Either x1 x2) z

lowerFun :: Fun a b -> a -> b
lowerFun (WrappedFun f) = f
lowerFun (EitherFun l r) = either l r

liftFun :: (a -> b) -> Fun a b
liftFun = WrappedFun

ffmap :: Functor f => Fun a b -> Fun (f a) (f b)
ffmap fab = liftFun (fmap (lowerFun fab))

ddimap :: MProfunctor p => (c -> a) -> (b -> d) -> p a b -> p c d
ddimap f g = mdimap (liftFun f) (liftFun g)

femap :: Fun a b -> Fun (Either z a) (Either z b)
femap f = EitherFun (Left . id) (Right . lowerFun f)

-- Note that (<.>) never destroys an inner EitherFun, that is good
-- for the free theorem as it means we are free to fuse dimaps whenever
-- we like without fear of destroying this chance. For if we had the chance
-- before, we still will afterwards.
(<.>) :: Fun b c -> Fun a b -> Fun a c
(<.>) cb (EitherFun l r) = EitherFun (lowerFun cb . l) (lowerFun cb . r)
(<.>) f g = liftFun (lowerFun f . lowerFun g)

(<.) :: Fun b c -> (a -> b) -> Fun a c
(<.) f g  = f <.> liftFun g

(.>) :: (b -> c) -> Fun a b -> Fun a c
(.>) f g = liftFun f <.> g

-------------------------------------------------------------------------------

class MProfunctor p where
  mdimap :: Fun c a -> Fun b d -> p a b -> p c d

class MRight p where
  mright :: p x y -> p (Either z x) (Either z y)


-------------------------------------------------------------------------------
-- Note [Optimisating Prisms Plan]
-- We want to write prisms in a normal form of
--    dimap f (EitherFun id g) . right
--
-- To that end, the optimisation plan is
--
-- 1. Float dimaps to the left and rights to the right, fusing together
--    dimaps as we go.
-- 2. Fuse together rights (taking care not to introduce EitherFun with
-- non-id first argument
-- 3. Apply the right free theorem to replace EitherFun f g with EitherFun
-- id g
--
-- There are three different adapters for this purpose
--
-- 1. DimapSyntax
-- 2. RightFuse
-- 3. RightFreeTheorem
--
-------------------------------------------------------------------------------
-- Debugging help

-- Run each stage individually

stage1,  stage3, stage4, stage5 :: (MProfunctor p, MRight p) => (forall p . (MProfunctor p, MRight p) => p a b) -> p a b
stage1 = lowerDimapSyntax
--stage2 = lowerPartialFusion
stage3 = lowerRightFreeTheorem
stage4 = lowerRightFuse
stage5 = lowerDimapK1

ravelStage1,  ravelStage3, ravelStage4, ravelStage5, ravelStage3a
  :: (MProfunctor p, MRight p)
        => (forall p . (MProfunctor p, MRight p) => p a b -> p s t) ->
          p a b -> p s t

ravelStage1 = ravelDimapSyntax
--ravelStage2 = ravelPartialFusion
ravelStage3 = ravelRightFreeTheorem
ravelStage3a = ravelDimapSyntaxFF
ravelStage4 = ravelRightFuse
ravelStage5 = ravelDimapK1

newtype FusionStack p a b =
  FusionStack { getFusionStack :: (DimapSyntax (RightFuse (RightFreeTheorem p)) a b) }

instance (MRight p, MProfunctor p) => MProfunctor (FusionStack p) where
  mdimap f g pab = FusionStack (mdimap f g (getFusionStack pab))
  {-# INLINE mdimap #-}

instance (MProfunctor p, MRight p) => MRight (FusionStack p) where
  mright pab = FusionStack (mright (getFusionStack pab))
  {-# INLINE mright #-}

liftFusionStack :: MProfunctor p => p a b -> FusionStack p a b
liftFusionStack = FusionStack . liftDimapSyntax . liftRightFuse . liftRightFreeTheorem

lowerFusionStack :: (MProfunctor p, MRight p) => FusionStack p a b -> p a b
lowerFusionStack = lowerRightFreeTheorem . lowerRightFuse . lowerDimapSyntax . getFusionStack
{-# INLINE lowerFusionStack #-}

ravelFusionStack :: (MProfunctor p, MRight p) =>
                 (FusionStack p a b -> FusionStack p s t)
                 -> p a b -> p s t
ravelFusionStack l pab = lowerFusionStack (l (liftFusionStack pab))
{-# INLINE ravelFusionStack #-}

{- This is morally correct but it doesn't optimise well,
- it is also pointless doing it like this as it introduces a lot of
- unecessary fmaps which we have to fuse together
ravelVLFusionStack :: (MProfunctor p, MRight p, Applicative f) =>
                 ((WrappedVLProfunctor (Boggle f) (FusionStack p)) a b
                    -> ((WrappedVLProfunctor (Boggle f) (FusionStack p)) s t))
                 -> p a (f b) -> p s (f t)
ravelVLFusionStack l pab =
      (lowerFusionStack (ddimap id lowerBoggle (wrappedVLProfunctor (
        (l (WrappedVLProfunctor (ddimap id liftBoggle (liftFusionStack pab))))))))
{-# INLINE ravelVLFusionStack #-}
-}

-- Works
--specFusionStack1 :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
--specFusionStack1 pab = ravelFusionStack testPartialFusion pab

specFusionStack2 :: (MProfunctor p, MRight p) => p Int Int -> p Int Int
specFusionStack2 pab = ravelFusionStack testRightFreeTheorem pab

-------------------------------------------------------------------------------
-- A wrapped profunctor which we can make an instance of our classes.
--

-- Use this adapter to get a profunctor lens
newtype WrappedProfunctor p a b = WrappedProfunctor { wrappedProfunctor :: (p a b) }

instance Profunctor p => MProfunctor (WrappedProfunctor p) where
  mdimap f g (WrappedProfunctor p) = WrappedProfunctor (dimap (lowerFun f) (lowerFun g) p)
  {-# INLINE mdimap #-}

instance Choice p => MRight (WrappedProfunctor p) where
  mright (WrappedProfunctor p) = WrappedProfunctor (right' p)
  {-# INLINE mright #-}

-- | Use this adapter to get a VL lens
newtype  WrappedVLMProfunctor f p a b
  = WrappedVLMProfunctor { wrappedVLMProfunctor :: p a (f b) }

instance (Functor f, MProfunctor p) => MProfunctor (WrappedVLMProfunctor f p) where
  mdimap f g (WrappedVLMProfunctor pafb)
    = WrappedVLMProfunctor (mdimap f (ffmap g) pafb)
  {-# INLINE mdimap #-}

instance (Applicative f, MRight p, MProfunctor p) => MRight (WrappedVLMProfunctor f p) where
  mright (WrappedVLMProfunctor pafb)
    = WrappedVLMProfunctor (mdimap (liftFun id) (EitherFun (pure . Left) (fmap Right)) (mright  pafb))
  {-# INLINE mright #-}

{-
-- | Use this adapter to get a VL lens
newtype  WrappedVLMProfunctor f p a b
  = WrappedVLMProfunctor { wrappedVLProfunctor :: p a (f b) }

instance (Functor f, MProfunctor p) => MProfunctor (WrappedVLMProfunctor f p) where
  mdimap f g (WrappedVLProfunctor pafb)
    = WrappedVLProfunctor (mdimap f (ffmap g) pafb)
  {-# INLINE mdimap #-}

instance (Applicative f, MRight p, MProfunctor p) => MRight (WrappedVLMProfunctor f p) where
  mright (WrappedVLMProfunctor pafb)
    = WrappedVLMProfunctor (ddimap id (either (pure . Left) (fmap Right)) (mright  pafb))
  {-# INLINE mright #-}
  -}



{-
instance MoreProfunctor p => MoreProfunctor (MoreBoggle p) where
  mdimap f g (MDirty pab) = MDMap f g pab
  mdimap f g (MDMap f' g' pab) = MDMap (f' <.> f) (g <.> g') pab
  mdimap f (EitherFun l r) (MDRight pab)
    = MDMap (liftFun (plus l id) <.> f) (EitherFun id r) (mright pab)
  mdimap f g (MDRight pab)     = MDMap f g (mright pab)

  mright (MDirty pab) = MDRight pab
  mright (MDMap f g pab) = MDMap (ffmap f) (ffmap g) (mright pab)
  mright (MDRight pab)   = MDMap assoc assoc' (mright pab)



data WInt = Wrap { unWrap :: !Int }

--test :: MoreProfunctor p => p Int Int -> p (Either WInt Int) (Either WInt Int)
--test pii = mdimap (PlusFun unWrap id) (liftFun id) (mplusp Wrap (mright pii))

test :: MoreProfunctor p => p Int Int -> p WInt WInt
test pii = mdimap (liftFun too) (EitherFun Wrap Wrap) (mright pii)
  where
    too :: WInt -> Either Int Int
    too (Wrap 0) = Left 0
    too (Wrap n) = Right n

spec1 :: forall p . Choice p => p Int Int -> p WInt WInt
spec1 eta = wrappedProfunctor (test (WrappedProfunctor eta))

spec :: Choice p => p Int Int -> p WInt WInt
spec eta = prismRavel test eta

injPrism :: MoreProfunctor p => MoreBoggle p s t -> p s t
injPrism (MDRight pa) = mright pa
injPrism (MDirty pa)  = pa
injPrism (MDMap f g pa) = mdimap f g pa
projPrism = MDirty


prismRavel :: (Choice p)
           => (MoreBoggle (WrappedProfunctor p) a b -> MoreBoggle (WrappedProfunctor p) s t)
           -> (p a b -> p s t)
prismRavel coy pafb = wrappedProfunctor (injPrism (coy (projPrism (WrappedProfunctor pafb))))
{-# INLINE prismRavel #-}
-}




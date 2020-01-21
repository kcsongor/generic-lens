{-# LANGUAGE TypeOperators #-}
module Data.Generics.Internal.Optic.Iso
  ( Sig.Iso
  , Sig.iso
  , Sig.iso2lens
  , Sig.iso2prism
  , Sig.set
  , Sig.view
  , (Sig.%)
  , Sig.refl

  , kIso
  , sumIso
  , mIso
  , repIso
  , prodIso
  , pairing
  , assoc3

  , fromIso
  ) where

import Data.Generics.Internal.Optic.Signatures.Iso as Sig

import GHC.Generics

kIso :: Iso (K1 r a p) (K1 r b p) a b
kIso = iso unK1 K1

sumIso :: Iso ((a :+: b) x) ((a' :+: b') x) (Either (a x) (b x)) (Either (a' x) (b' x))
sumIso = iso back forth
  where forth (Left l)  = L1 l
        forth (Right r) = R1 r
        back (L1 l) = Left l
        back (R1 r) = Right r
{-# INLINE sumIso #-}

-- | 'M1' is just a wrapper around `f p`
--mIso :: Iso' (M1 i c f p) (f p)
mIso :: Iso (M1 i c f p) (M1 i c g p) (f p) (g p)
mIso = iso unM1 M1
{-# INLINE mIso #-}

-- | A type and its generic representation are isomorphic
repIso :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b x)
repIso = iso from to

fromIso :: Iso s t a b -> Iso b a t s
fromIso l = withIso l $ \ sa bt -> iso bt sa
{-# INLINE fromIso #-}

prodIso :: Iso ((a :*: b) x) ((a' :*: b') x) (a x, b x) (a' x, b' x)
prodIso = iso (\(a :*: b) -> (a, b)) (\(a, b) -> (a :*: b))

pairing :: Iso s t a b -> Iso s' t' a' b' -> Iso (s, s') (t, t') (a, a') (b, b')
pairing f g = withIso f $ \ sa bt -> withIso g $ \s'a' b't' ->
  iso (bmap sa s'a') (bmap bt b't')
  where bmap f' g' (a, b) = (f' a, g' b)

assoc3 :: Iso ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3 = iso (\((a, b), c) -> (a, (b, c))) (\(a, (b, c)) -> ((a, b), c))

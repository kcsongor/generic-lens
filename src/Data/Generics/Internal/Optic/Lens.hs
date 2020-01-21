{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module Data.Generics.Internal.Optic.Lens
  ( Sig.Lens
  , Sig.lens
  , (Sig.%)
  , Sig.view
  , Sig.modify
  , Sig.set
  , _2

  , first
  , second
  , choosing
  ) where

import Data.Generics.Internal.Optic.Signatures.Lens as Sig

import GHC.Generics

-- | Lens focusing on the first element of a product
first :: Lens ((a :*: b) x) ((a' :*: b) x) (a x) (a' x)
first
  = lens (\(a :*: _) -> a) (\(_ :*: b) a' -> a' :*: b)

-- | Lens focusing on the second element of a product
second :: Lens ((a :*: b) x) ((a :*: b') x) (b x) (b' x)
second
  = lens (\(_ :*: b) -> b) (\(a :*: _) b' -> a :*: b')

_2 :: Lens (x, a) (x, b) a b
_2
  = lens (\(_, b) -> b) (\(a, _) b' -> (a, b'))

choosing :: forall s t a b s' t' . Lens s t a b -> Lens s' t' a b -> Lens (Either s s') (Either t t') a b
choosing l r = withLensPrim l (\getl setl ->
                  withLensPrim r (\getr setr ->
                            let --g :: Either s s' -> a
                                g e = case e of
                                        Left v -> getl v
                                        Right v -> getr v
                                s e v' = case e of
                                           Left v -> Left (setl v v')
                                           Right v -> Right (setr v v')
                            in lens g s))

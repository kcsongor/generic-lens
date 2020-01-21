{-# LANGUAGE TypeOperators #-}
module Data.Generics.Internal.Optic.Prism
  ( Sig.Prism
  , Sig.prism
  , (Sig.%)
  , Sig.build
  , Sig.match

  , left
  , right
  ) where

import Data.Generics.Internal.Optic.Signatures.Prism as Sig

import GHC.Generics

gsum :: (a x -> c) -> (b x -> c) -> ((a :+: b) x) -> c
gsum f _ (L1 x) =  f x
gsum _ g (R1 y) =  g y

left :: Prism ((a :+: c) x) ((b :+: c) x) (a x) (b x)
left = prism L1 $ gsum Right (Left . R1)

right :: Prism ((a :+: b) x) ((a :+: c) x) (b x) (c x)
right = prism R1 $ gsum (Left . L1) Right

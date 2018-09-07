{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Data.Generics.Iso
  ( Wrapped(..)
  , _Unwrapped
  , _Wrapped
  )
where

import Data.Generics.Internal.VL.Iso as VL
import Data.Generics.Internal.VL.Lens ((^.))

import Data.Kind (Constraint)
import GHC.Generics
import GHC.TypeLits

class GWrapped a b | a -> b where
  gWrapped :: Iso' (a x) b

instance GWrapped a b => GWrapped (M1 i k a) b where
  gWrapped = mIso.gWrapped

instance GWrapped (K1 i c) c where
  gWrapped = kIso

class Wrapped a b | a -> b where
  {-# minimal wrappedIso | wrappedTo, wrappedFrom #-}
  wrappedIso :: VL.Iso' a b
  wrappedIso = iso wrappedTo wrappedFrom

  wrappedTo :: a -> b
  wrappedTo a = a ^. wrappedIso

  wrappedFrom :: b -> a
  wrappedFrom a = a ^. fromIso wrappedIso

type family ErrorUnlessOnlyOne a b :: Constraint where
  ErrorUnlessOnlyOne t (M1 i k a) = ErrorUnlessOnlyOne t a
  ErrorUnlessOnlyOne t (K1 i a) = ()
  ErrorUnlessOnlyOne t a =
    TypeError ('ShowType t ':<>: 'Text " is not a single-constructor, single-field datatype")

instance (Generic a, ErrorUnlessOnlyOne a (Rep a), GWrapped (Rep a) b) => Wrapped a b where
  wrappedIso = repIso . gWrapped

_Unwrapped :: Wrapped a b => Iso' a b
_Unwrapped = wrappedIso

_Wrapped :: Wrapped a b => Iso' b a
_Wrapped = fromIso wrappedIso

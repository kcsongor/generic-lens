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
  ( Is(..)
  , _Unwrapped
  , _Wrapped
  )
where

import Data.Generics.Internal.VL.Iso as VL
import Data.Generics.Internal.VL.Lens ((^.))

import Data.Kind (Constraint)
import GHC.Generics
import GHC.TypeLits

class GIso a b | a -> b where
  gIso :: Iso' (a x) b

instance GIso a b => GIso (M1 i k a) b where
  gIso = mIso.gIso

instance GIso (K1 i c) c where
  gIso = kIso

class Is a b | a -> b where
  {-# minimal _Is | isTo, isFrom #-}
  _Is :: VL.Iso' a b
  _Is = iso isTo isFrom

  isTo :: a -> b
  isTo a = a ^. _Is

  isFrom :: b -> a
  isFrom a = a ^. fromIso _Is

type family ErrorUnlessOnlyOne a b :: Constraint where
  ErrorUnlessOnlyOne t (M1 i k a) = ErrorUnlessOnlyOne t a
  ErrorUnlessOnlyOne t (K1 i a) = ()
  ErrorUnlessOnlyOne t a =
    TypeError ('ShowType t ':<>: 'Text " is not a single-constructor, single-field datatype")

instance (Generic a, ErrorUnlessOnlyOne a (Rep a), GIso (Rep a) b) => Is a b where
  _Is = repIso . gIso

_Unwrapped :: Is a b => Iso' a b
_Unwrapped = _Is

_Wrapped :: Is a b => Iso' b a
_Wrapped = fromIso _Is

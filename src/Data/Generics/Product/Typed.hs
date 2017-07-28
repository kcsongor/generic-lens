{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Generics.Product.Typed
  ( HasType (..)

  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics
import GHC.TypeLits

class HasType a s where
  typed :: Lens' s a

class GHasType (f :: Type -> Type) a where
  gtyped :: Lens' (f x) a

instance (GHasType l a, GHasType r a) => GHasType (l :+: r) a where
  gtyped = combine (gtyped @l) (gtyped @r)

instance GHasType (S1 meta (Rec0 a)) a where
  gtyped = mIso . gtyped

instance GHasType (K1 R a) a where
  gtyped f (K1 x) = fmap K1 (f x)

instance GHasType f a => GHasType (M1 D meta f) a where
  gtyped = mIso . gtyped

instance GHasType f a => GHasType (M1 C meta f) a where
  gtyped = mIso . gtyped

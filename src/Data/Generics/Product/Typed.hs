{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Generics.Product.Typed
  ( HasType (..)

  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics

class HasType a s where
  typed :: Lens' s a

instance (Generic s,
          FindType a (Rep s) ~ 'Just a,
          CountType a (Rep s) ~ 'One,
          GHasType (Rep s) a)

      =>  HasType a s where

  typed = repIso . gtyped

class GHasType (f :: Type -> Type) a where
  gtyped :: Lens' (f x) a

instance GProductHasType l r a (FindType a l)
      => GHasType (l :*: r) a where

  gtyped = gproductTyped @_ @_ @_ @(FindType a l)

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

class GProductHasType l r a (found :: Maybe Type) where
  gproductTyped :: Lens' ((l :*: r) x) a

instance GHasType l a => GProductHasType l r a ('Just a) where
  gproductTyped = first . gtyped

instance GHasType r a => GProductHasType l r a 'Nothing where
  gproductTyped = second . gtyped

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Generics.Product.Fields
  ( HasField (..)

  , GHasField (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

import Data.Kind    (Constraint, Type)
import GHC.Generics
import GHC.TypeLits

class HasField (field :: Symbol) a s | s field -> a where
  field :: Lens' s a

instance (Generic s,
          found ~ FindField field (Rep s),
          ErrorUnlessJust field s found,
          found ~ 'Just a,
          GHasField field (Rep s) a)

      =>  HasField field a s where

  field = repIso . gfield @field

type family ErrorUnlessJust (field :: Symbol) (s :: Type) (found :: Maybe Type) :: Constraint where
  ErrorUnlessJust field s 'Nothing
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field named "
        ':<>: 'ShowType field
        )

  ErrorUnlessJust _ _ ('Just _)
    = ()

class GHasField (field :: Symbol) (f :: Type -> Type) a | field f -> a where
  gfield :: Lens' (f x) a

instance GProductHasField field l r a (FindField field l)
      => GHasField field (l :*: r) a where

  gfield = gproductField @field @_ @_ @_ @(FindField field l)

instance (GHasField field l a, GHasField field r a)
      =>  GHasField field (l :+: r) a where

  gfield = combine (gfield @field @l) (gfield @field @r)

instance GHasField field (S1 ('MetaSel ('Just field) upkd str infstr) (Rec0 a)) a where
  gfield = mIso . gfield @field

instance GHasField field (K1 R a) a where
  gfield f (K1 x) = fmap K1 (f x)

instance GHasField field f a => GHasField field (M1 D meta f) a where
  gfield = mIso . gfield @field

instance GHasField field f a => GHasField field (M1 C meta f) a where
  gfield = mIso . gfield @field

class GProductHasField (field :: Symbol) l r a (found :: Maybe Type) | field l r -> a where
  gproductField :: Lens' ((l :*: r) x) a

instance GHasField field l a => GProductHasField field l r a ('Just a) where
  gproductField = first . gfield @field

instance GHasField field r a => GProductHasField field l r a 'Nothing where
  gproductField = second . gfield @field

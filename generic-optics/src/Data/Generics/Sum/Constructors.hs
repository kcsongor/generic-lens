{-# LANGUAGE PackageImports #-}
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
{-# LANGUAGE FlexibleContexts       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Constructors
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-name-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Constructors
  ( -- *Prisms

    -- $setup
    AsConstructor (..)
  , AsConstructor_ (..)
  , AsConstructor' (..)
  , AsConstructor0 (..)
  ) where

import "this" Data.Generics.Internal.Optics

import "generic-lens-core" Data.Generics.Internal.Void
import qualified "generic-lens-core" Data.Generics.Sum.Internal.Constructors as Core

import GHC.TypeLits (Symbol)


-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> import GHC.Generics
-- >>> import Optics.Core
-- >>> :m +Data.Generics.Product.Fields
-- >>> :m +Data.Function
-- >>> :{
-- data Animal a
--   = Dog (Dog a)
--   | Cat Name Age
--   | Duck Age
--   deriving (Generic, Show)
-- data Dog a
--   = MkDog
--   { name   :: Name
--   , age    :: Age
--   , fieldA :: a
--   }
--   deriving Show
-- type Name = String
-- type Age  = Int
-- dog, cat, duck :: Animal Int
-- dog = Dog (MkDog "Shep" 3 30)
-- cat = Cat "Mog" 5
-- duck = Duck 2
-- :}

-- |Sums that have a constructor with a given name.
class AsConstructor (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b where
  -- |A prism that projects a named constructor from a sum.
  --
  --  >>> dog ^? _Ctor @"Dog"
  --  Just (MkDog {name = "Shep", age = 3, fieldA = 30})
  --
  --  >>> dog ^? _Ctor @"Cat"
  --  Nothing
  --
  --  >>> cat ^? _Ctor @"Cat"
  --  Just ("Mog",5)
  --
  --  >>> _Ctor @"Cat" # ("Garfield", 6) :: Animal Int
  --  Cat "Garfield" 6
  --
  --  === /Type errors/
  --
  --  >>> cat ^? _Ctor @"Turtle"
  --  ...
  --  ...
  --  ... The type Animal Int does not contain a constructor named "Turtle"
  --  ...
  _Ctor :: Prism s t a b

-- |Sums that have a constructor with a given name.
--
-- The difference between 'HasConstructor' and 'HasConstructor_' is similar to
-- the one between 'Data.Generics.Product.Fields.HasField' and
-- 'Data.Generics.Product.Fields.HasField_'.
-- See 'Data.Generics.Product.Fields.HasField_'.
class AsConstructor_ (ctor :: Symbol) s t a b where
  _Ctor_ :: Prism s t a b

class AsConstructor' (ctor :: Symbol) s a | ctor s -> a where
  _Ctor' :: Prism s s a a

-- |Sums that have a constructor with a given name.
--
-- This class gives the minimal constraints needed to define this prism.
-- For common uses, see 'HasConstructor'.
class AsConstructor0 (ctor :: Symbol) s t a b where
  _Ctor0 :: Prism s t a b

instance (Core.Context' ctor s a, AsConstructor0 ctor s s a a) => AsConstructor' ctor s a where
  _Ctor' = _Ctor0 @ctor
  {-# INLINE _Ctor' #-}

instance (Core.Context ctor s t a b, AsConstructor0 ctor s t a b) => AsConstructor ctor s t a b where
  _Ctor = _Ctor0 @ctor
  {-# INLINE _Ctor #-}

-- | See Note [Uncluttering type signatures]
--
-- >>> :t _Ctor
-- _Ctor :: AsConstructor ctor s t a b => Prism s t a b
instance {-# OVERLAPPING #-} AsConstructor ctor (Void1 a) (Void1 b) a b where
  _Ctor = undefined

instance (Core.Context_ ctor s t a b, AsConstructor0 ctor s t a b) => AsConstructor_ ctor s t a b where
  _Ctor_ = _Ctor0 @ctor
  {-# INLINE _Ctor_ #-}

instance {-# OVERLAPPING #-} AsConstructor_ ctor (Void1 a) (Void1 b) a b where
  _Ctor_ = undefined

instance Core.Context0 ctor s t a b => AsConstructor0 ctor s t a b where
  _Ctor0 = normalisePrism (Optic (Core.derived0 @ctor))
  {-# INLINE _Ctor0 #-}


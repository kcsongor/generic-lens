{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Any
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive a variety of prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Any
  ( -- *Prisms
    --
    -- $setup
    AsAny (..)
  ) where

import "this" Data.Generics.Internal.Optics
import "this" Data.Generics.Sum.Constructors
import "this" Data.Generics.Sum.Typed

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> import Optics.Core
-- >>> :{
-- data Animal
--   = Dog Dog
--   | Cat Name Age
--   | Duck Age
--   deriving (Generic, Show)
-- data Dog
--   = MkDog
--   { name :: Name
--   , age  :: Age
--   }
--   deriving (Generic, Show)
-- type Name = String
-- type Age  = Int
-- dog, cat, duck :: Animal
-- dog = Dog (MkDog "Shep" 3)
-- cat = Cat "Mog" 5
-- duck = Duck 2
-- :}

-- |Sums that have generic prisms.
class AsAny sel a s | s sel -> a where
  -- |A prism that projects a sum as identified by some selector. Currently
  --  supported selectors are constructor names and unique types.
  --
  --  >>> dog ^? _As @"Dog"
  --  Just (MkDog {name = "Shep", age = 3})
  --
  --  >>> dog ^? _As @Dog
  --  Just (MkDog {name = "Shep", age = 3})
  --
  --  >>> dog ^? _As @"Cat"
  --  Nothing
  --
  --  >>> cat ^? _As @(Name, Age)
  --  Just ("Mog",5)
  --
  --  >>> cat ^? _As @"Cat"
  --  Just ("Mog",5)
  --
  --  >>> _As @"Cat" # ("Garfield", 6) :: Animal
  --  Cat "Garfield" 6
  --
  --  >>> duck ^? _As @Age
  --  Just 2
  _As :: Prism s s a a

instance AsConstructor ctor s s a a => AsAny ctor a s where
  _As = _Ctor @ctor

instance AsType a s => AsAny a a s where
  _As = _Typed @a


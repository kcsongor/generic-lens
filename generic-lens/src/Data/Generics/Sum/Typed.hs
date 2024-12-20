{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Typed
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-field-type-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Typed
  ( -- *Prisms
    --
    -- $setup
    AsType (..)
  ) where

import "this" Data.Generics.Internal.VL.Prism

import qualified "generic-lens-core" Data.Generics.Sum.Internal.Typed as Core
import "generic-lens-core" Data.Generics.Internal.Void

-- $setup
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> import Control.Lens
-- >>> :{
-- data Animal
--   = Dog Dog
--   | Cat Name Age
--   | Duck Age
--   | Turtle Age
--   deriving (Generic, Show)
-- data Dog
--   = MkDog
--   { name :: Name
--   , age  :: Age
--   }
--   deriving (Generic, Show)
-- type Name = String
-- newtype Age  = Age Int deriving Show
-- dog, cat, duck :: Animal
-- dog = Dog (MkDog "Shep" (Age 3))
-- cat = Cat "Mog" (Age 5)
-- duck = Duck (Age 2)
-- >>> :}


-- |Types that can represent another type, either by being a sum with a
-- constructor containing that type, or by actually being that type.
class AsType a s where
  -- |A prism that projects a constructor uniquely identifiable by the type of
  --  its field. Compatible with the lens package's 'Control.Lens.Prism' type.
  --
  --  >>> dog ^? _Typed @Dog
  --  Just (MkDog {name = "Shep", age = Age 3})
  --  >>> cat ^? _Typed @(Name, Age)
  --  Just ("Mog",Age 5)
  --  >>> dog ^? _Typed @Age
  --  ...
  --  ...
  --  ... The type Animal contains multiple constructors whose fields are of type Age.
  --  ... The choice of constructor is thus ambiguous, could be any of:
  --  ... Duck
  --  ... Turtle
  --  ...
  _Typed :: Prism' s a
  _Typed = prism injectTyped (\i -> maybe (Left i) Right (projectTyped i))
  {-# INLINE _Typed #-}

  -- |Inject by type.
  --
  --
  --  >>> :{
  --  dog :: Dog
  --  dog = MkDog "Fido" (Age 11)
  --  dogAsAnimal :: Animal
  --  dogAsAnimal = injectTyped dog
  --  dogAsItself :: Dog
  --  dogAsItself = injectTyped dog
  -- >>> :}
  injectTyped :: a -> s
  injectTyped
    = build _Typed

  -- |Project by type.
  --
  --
  -- >>> :{
  -- dogAsAnimal :: Animal
  -- dogAsAnimal = Dog (MkDog "Fido (Age 11)")
  -- mDog :: Maybe Dog
  -- mDog = projectTyped dogAsAnimal
  -- >>> :}
  projectTyped :: s -> Maybe a
  projectTyped
    = either (const Nothing) Just . match _Typed

  {-# MINIMAL (injectTyped, projectTyped) | _Typed #-}

-- |Every type can be treated 'as' itself.
instance {-# OVERLAPPING #-} AsType a a where
  injectTyped = id
  projectTyped = Just

instance Core.Context a s => AsType a s where
  _Typed eta = prism2prismvl Core.derived eta
  {-# INLINE _Typed #-}

-- | Uncluttering type signatures (see 'Void')
--
-- >>> :t +d _Typed
-- _Typed
--   :: (AsType a s, Choice p, Applicative f) => p a (f a) -> p s (f s)
instance {-# OVERLAPPING #-} AsType a Void where
  _Typed = undefined
  injectTyped = undefined
  projectTyped = undefined

-- | Uncluttering type signatures (see 'Void')
--
-- >>> :t +d _Typed @Int
-- _Typed @Int
--   :: (AsType Int s, Choice p, Applicative f) =>
--      p Int (f Int) -> p s (f s)
instance {-# OVERLAPPING #-} AsType Void a where
  _Typed = undefined
  injectTyped = undefined
  projectTyped = undefined


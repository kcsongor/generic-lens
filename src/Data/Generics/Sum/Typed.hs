{-# LANGUAGE CPP #-}
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
    --  $setup
    AsType (..)
  ) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits (TypeError, ErrorMessage (..), Symbol)
import Data.Generics.Sum.Internal.Typed
import Data.Generics.Internal.Errors

import Data.Generics.Internal.Families
import Data.Generics.Internal.Void
import Data.Generics.Product.Internal.HList
import Data.Generics.Internal.VL.Prism
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism (prismPRavel)

-- $setup
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.VL.Prism
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
-- :}


-- |Sums that have a constructor with a field of the given type.
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
  {-# INLINE[2] _Typed #-}

  -- |Inject by type.
  injectTyped :: a -> s
  injectTyped
    = build _Typed

  -- |Project by type.
  projectTyped :: s -> Maybe a
  projectTyped
    = either (const Nothing) Just . match _Typed

  {-# MINIMAL (injectTyped, projectTyped) | _Typed #-}

instance
  ( Generic s
  , ErrorUnlessOne a s (CollectPartialType as (Rep s))
  , as ~ TupleToList a
  , ListTuple a as
  , GAsType (Rep s) as
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic prism focusing on a constructor of type " ':<>: QuoteType a])
    (() :: Constraint)
  ) => AsType a s where

  _Typed eta = prismRavel (prismPRavel (repIso . _GTyped @_ @as . tupled)) eta
  {-# INLINE[2] _Typed #-}

-- | See Note [Uncluttering type signatures]
#if __GLASGOW_HASKELL__ < 804
-- >>> :t _Typed
-- _Typed
--   :: (Applicative f, Data.Profunctor.Choice.Choice p, AsType a s) =>
--      p a (f a) -> p s (f s)
#else
-- >>> :t _Typed
-- _Typed
--   :: (AsType a s, Data.Profunctor.Choice.Choice p, Applicative f) =>
--      p a (f a) -> p s (f s)
#endif
instance {-# OVERLAPPING #-} AsType a Void where
  _Typed = undefined
  injectTyped = undefined
  projectTyped = undefined

-- | See Note [Uncluttering type signatures]
#if __GLASGOW_HASKELL__ < 804
-- >>> :t _Typed @Int
-- _Typed @Int
--   :: (Applicative f, Data.Profunctor.Choice.Choice p,
--       AsType Int s) =>
--      p Int (f Int) -> p s (f s)
#else
-- >>> :t _Typed @Int
-- _Typed @Int
--   :: (AsType Int s, Data.Profunctor.Choice.Choice p,
--       Applicative f) =>
--      p Int (f Int) -> p s (f s)
#endif
instance {-# OVERLAPPING #-} AsType Void a where
  _Typed = undefined
  injectTyped = undefined
  projectTyped = undefined

type family ErrorUnlessOne (a :: Type) (s :: Type) (ctors :: [Symbol]) :: Constraint where
  ErrorUnlessOne _ _ '[_]
    = ()

  ErrorUnlessOne a s '[]
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a constructor whose field is of type "
        ':<>: 'ShowType a
        )

  ErrorUnlessOne a s cs
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " contains multiple constructors whose fields are of type "
        ':<>: 'ShowType a ':<>: 'Text "."
        ':$$: 'Text "The choice of constructor is thus ambiguous, could be any of:"
        ':$$: ShowSymbols cs
        )

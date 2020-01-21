{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Subtype
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structural subtype relationships between product types.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Subtype
  ( -- *Lenses
    --
    -- $setup
    Subtype (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.VL.Lens as VL
import Data.Generics.Internal.Void
import Data.Generics.Product.Internal.Subtype

import GHC.Generics (Generic (Rep, to, from) )
import GHC.TypeLits (Symbol, TypeError, ErrorMessage (..))
import Data.Kind (Type, Constraint)
import Data.Generics.Internal.Errors

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XDuplicateRecordFields
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.VL.Lens
-- >>> :{
-- data Human = Human
--   { name    :: String
--   , age     :: Int
--   , address :: String
--   }
--   deriving (Generic, Show)
-- data Animal = Animal
--   { name    :: String
--   , age     :: Int
--   }
--   deriving (Generic, Show)
-- human :: Human
-- human = Human "Tunyasz" 50 "London"
-- :}

-- |Structural subtype relationship
--
-- @sub@ is a (structural) `subtype' of @sup@, if its fields are a subset of
-- those of @sup@.
--
class Subtype sup sub where
  -- |Structural subtype lens. Given a subtype relationship @sub :< sup@,
  --  we can focus on the @sub@ structure of @sup@.
  --
  -- >>> human ^. super @Animal
  -- Animal {name = "Tunyasz", age = 50}
  --
  -- >>> set (super @Animal) (Animal "dog" 10) human
  -- Human {name = "dog", age = 10, address = "London"}
  super  :: VL.Lens sub sub sup sup
  super
    = VL.lens upcast (flip smash)

  -- |Cast the more specific subtype to the more general supertype
  --
  -- >>> upcast human :: Animal
  -- Animal {name = "Tunyasz", age = 50}
  --
  -- >>> upcast (upcast human :: Animal) :: Human
  -- ...
  -- ... The type 'Animal' is not a subtype of 'Human'.
  -- ... The following fields are missing from 'Animal':
  -- ... address
  -- ...
  upcast :: sub -> sup
  upcast s = s ^. super @sup

  -- |Plug a smaller structure into a larger one
  --
  -- >>> smash (Animal "dog" 10) human
  -- Human {name = "dog", age = 10, address = "London"}
  smash  :: sup -> sub -> sub
  smash = VL.set (super @sup)

  {-# MINIMAL super | smash, upcast #-}

instance
  ( Generic a
  , Generic b
  , GSmash (Rep a) (Rep b)
  , GUpcast (Rep a) (Rep b)
  , CustomError a b
  ) => Subtype b a where
    smash p b = to $ gsmash (from p) (from b)
    upcast    = to . gupcast . from

type family CustomError a b :: Constraint where
  CustomError a b =
    ( ErrorUnless b a (CollectFieldsOrdered (Rep b) \\ CollectFieldsOrdered (Rep a))
    , Defined (Rep a)
      (NoGeneric a '[ 'Text "arising from a generic lens focusing on " ':<>: QuoteType b
                    , 'Text "as a supertype of " ':<>: QuoteType a
                    ])
      (() :: Constraint)
    , Defined (Rep b)
      (NoGeneric b '[ 'Text "arising from a generic lens focusing on " ':<>: QuoteType b
                    , 'Text "as a supertype of " ':<>: QuoteType a
                    ])
      (() :: Constraint)
    )

instance {-# OVERLAPPING #-} Subtype a a where
  super = id

-- | See Note [Uncluttering type signatures]
#if __GLASGOW_HASKELL__ < 804
-- >>> :t super
-- super
--   :: (Subtype sup sub, Functor f) => (sup -> f sup) -> sub -> f sub
#else
-- >>> :t super
-- super
--   :: (Functor f, Subtype sup sub) => (sup -> f sup) -> sub -> f sub
#endif
instance {-# OVERLAPPING #-} Subtype a Void where
  super = undefined

-- | See Note [Uncluttering type signatures]
#if __GLASGOW_HASKELL__ < 804
-- >>> :t super @Int
-- super @Int
--   :: (Subtype Int sub, Functor f) => (Int -> f Int) -> sub -> f sub
#else
-- >>> :t super @Int
-- super @Int
--   :: (Functor f, Subtype Int sub) => (Int -> f Int) -> sub -> f sub
#endif
instance {-# OVERLAPPING #-} Subtype Void a where
  super = undefined

type family ErrorUnless (sup :: Type) (sub :: Type) (diff :: [Symbol]) :: Constraint where
  ErrorUnless _ _ '[]
    = ()

  ErrorUnless sup sub fs
    = TypeError
        (     'Text "The type '"
        ':<>: 'ShowType sub
        ':<>: 'Text "' is not a subtype of '"
        ':<>: 'ShowType sup ':<>: 'Text "'."
        ':$$: 'Text "The following fields are missing from '"
        ':<>: 'ShowType sub ':<>: 'Text "':"
        ':$$: ShowSymbols fs
        )

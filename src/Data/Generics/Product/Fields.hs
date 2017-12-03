{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Fields
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive record field getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Fields
  ( -- *Lenses

    --  $setup
    HasField (..)
  , HasField'

  , getField
  , setField
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens
import Data.Generics.Internal.Void
import Data.Generics.Product.Internal.Fields

import Data.Kind    (Constraint, Type)
import GHC.Generics
import GHC.TypeLits (Symbol, ErrorMessage(..), TypeError)

-- $setup
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.Lens
-- >>> :m +Data.Function
-- >>> :{
-- data Human a = Human
--   { name    :: String
--   , age     :: Int
--   , address :: String
--   , other   :: a
--   }
--   deriving (Generic, Show)
-- human :: Human Bool
-- human = Human { name = "Tunyasz", age = 50, address = "London", other = False }
-- :}

-- |Records that have a field with a given name.
class HasField (field :: Symbol) s t a b | s field -> a where
  -- |A lens that focuses on a field with a given name. Compatible with the
  --  lens package's 'Control.Lens.Lens' type.
  --
  --  >>> human ^. field @"age"
  --  50
  --
  -- If the field's type comes from a type parameter, it can be changed:
  --  >>> :t human
  --  human :: Human Bool
  --  >>> :t human & field @"other" .~ 42
  --  human & field @"other" .~ 42 :: Num p => Human p
  --  >>> human & field @"other" .~ 42
  --  Human {name = "Tunyasz", age = 50, address = "London", other = 42}
  field :: Lens s t a b

type HasField' field s a = HasField field s s a a

-- |
-- >>> getField @"age" human
-- 50
getField :: forall f s a. HasField' f s a => s -> a
getField s = s ^. field @f

-- |
-- >>> setField @"age" 60 human
-- Human {name = "Tunyasz", age = 60, address = "London", other = False}
setField :: forall f s a. HasField' f s a => a -> s -> s
setField = set (field @f)

instance  -- see Note [Changing type parameters]
  ( Generic s
  , ErrorUnless field s (HasTotalFieldP field (Rep s))
  , Generic t
  , s' ~ Proxied s
  , Generic s'
  , GHasField' field (Rep s) a
  , GHasField' field (Rep s') a'
  , GHasField field (Rep s) (Rep t) a b
  , '(t, b) ~ Infer s' a' a p
  ) => HasField field s t a b where

  field f s = ravel (repLens . gfield @field) f s

-- See Note [Uncluttering type signatures]
instance {-# OVERLAPPING #-} HasField f (Void2 t a) t a b where
  field = undefined

type family ErrorUnless (field :: Symbol) (s :: Type) (contains :: Bool) :: Constraint where
  ErrorUnless field s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field named "
        ':<>: 'ShowType field
        )

  ErrorUnless _ _ 'True
    = ()

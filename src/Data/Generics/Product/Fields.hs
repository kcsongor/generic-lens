{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE CPP                     #-}
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

    -- $setup
    HasField (..)
  , HasField'

  , getField
  , setField
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.VL.Lens as VL
import Data.Generics.Internal.Void
import Data.Generics.Product.Internal.Keyed

import Data.Kind    (Constraint, Type)
import GHC.Generics
import GHC.TypeLits (Symbol, ErrorMessage(..), TypeError)
import Data.Generics.Internal.Profunctor.Lens as P

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.VL.Lens
-- >>> :m +Data.Function
-- >>> :{
-- data Human a
--   = Human
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     , other   :: a
--     }
--   | HumanNoAddress
--     { name    :: String
--     , age     :: Int
--     , other   :: a
--     }
--   deriving (Generic, Show)
-- human :: Human Bool
-- human = Human { name = "Tunyasz", age = 50, address = "London", other = False }
-- :}

-- |Records that have a field with a given name.
class HasField (field :: Symbol) s t a b | s field -> a, t field -> b, s field b -> t, t field a -> s where
  -- |A lens that focuses on a field with a given name. Compatible with the
  --  lens package's 'Control.Lens.Lens' type.
  --
  --  >>> human ^. field @"age"
  --  50
  --
  --  === /Type changing/
  --
  --  >>> :t human
  --  human :: Human Bool
  --
  --  >>> :t human & field @"other" .~ (42 :: Int)
  --  human & field @"other" .~ (42 :: Int) :: Human Int
  --
  --  >>> human & field @"other" .~ 42
  --  Human {name = "Tunyasz", age = 50, address = "London", other = 42}
  --
  --  === /Type errors/
  --
  --  >>> human & field @"weight" .~ 42
  --  ...
  --  ... The type Human Bool does not contain a field named 'weight'.
  --  ...
  --
  --  >>> human & field @"address" .~ ""
  --  ...
  --  ... Not all constructors of the type Human Bool
  --  ... contain a field named 'address'.
  --  ... The offending constructors are:
  --  ... HumanNoAddress
  --  ...
  field :: VL.Lens s t a b

type HasField' field s a = HasField field s s a a

-- |
-- >>> getField @"age" human
-- 50
getField :: forall f a s.  HasField' f s a => s -> a
getField = VL.view (field @f)

-- |
-- >>> setField @"age" 60 human
-- Human {name = "Tunyasz", age = 60, address = "London", other = False}
setField :: forall f s a. HasField' f s a => a -> s -> s
setField = VL.set (field @f)

instance  -- see Note [Changing type parameters]
  ( Generic s
  , ErrorUnless field s (CollectField field (Rep s))
  , Generic t
  -- see Note [CPP in instance constraints]
#if __GLASGOW_HASKELL__ < 802
  , '(s', t') ~ '(Proxied s, Proxied t)
#else
  , s' ~ Proxied s
  , t' ~ Proxied t
#endif
  , Generic s'
  , Generic t'
  , GHasKey' field (Rep s) a
  , GHasKey' field (Rep s') a'
  , GHasKey' field (Rep t') b'
  , GHasKey  field (Rep s) (Rep t) a b
  , t ~ Infer s a' b
  , s ~ Infer t b' a
  ) => HasField field s t a b where

  field f s = VL.ravel (repLens . gkey @field) f s

-- -- See Note [Uncluttering type signatures]
instance {-# OVERLAPPING #-} HasField f (Void1 a) (Void1 b) a b where
  field = undefined

type family ErrorUnless (field :: Symbol) (s :: Type) (stat :: TypeStat) :: Constraint where
  ErrorUnless field s ('TypeStat _ _ '[])
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field named '"
        ':<>: 'Text field ':<>: 'Text "'."
        )

  ErrorUnless field s ('TypeStat (n ': ns) _ _)
    = TypeError
        (     'Text "Not all constructors of the type "
        ':<>: 'ShowType s
        ':$$: 'Text " contain a field named '"
        ':<>: 'Text field ':<>: 'Text "'."
        ':$$: 'Text "The offending constructors are:"
        ':$$: ShowSymbols (n ': ns)
        )

  ErrorUnless _ _ ('TypeStat '[] '[] _)
    = ()

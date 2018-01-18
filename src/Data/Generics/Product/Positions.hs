{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Positions
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive positional product type getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Positions
  ( -- *Lenses

    -- $setup
    HasPosition (..)
  , HasPosition'

  , getPosition
  , setPosition
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Internal.Void
import Data.Generics.Internal.Families
import Data.Generics.Product.Internal.Positions

import Data.Kind      (Constraint, Type)
import Data.Type.Bool (type (&&))
import GHC.Generics
import GHC.TypeLits   (type (<=?),  Nat, TypeError, ErrorMessage(..))

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.Lens
-- >>> :m +Data.Function
-- >>> :{
-- data Human = Human
--   { name    :: String
--   , age     :: Int
--   , address :: String
--   }
--   deriving (Generic, Show)
-- human :: Human
-- human = Human "Tunyasz" 50 "London"
-- :}

-- |Records that have a field at a given position.
class HasPosition (i :: Nat) s t a b | s i -> a, t i -> b, s i b -> t, t i a -> s where
  -- |A lens that focuses on a field at a given position. Compatible with the
  --  lens package's 'Control.Lens.Lens' type.
  --
  --  >>> human ^. position @1
  --  "Tunyasz"
  --  >>> human & position @3 .~ "Berlin"
  --  Human {name = "Tunyasz", age = 50, address = "Berlin"}
  --
  --  === /Type errors/
  --
  --  >>> human & position @4 .~ "Berlin"
  --  ...
  --  ... The type Human does not contain a field at position 4
  --  ...
  position :: LensVL s t a b

type HasPosition' i s a = HasPosition i s s a a

getPosition :: forall i s a. HasPosition' i s a => s -> a
getPosition s = s ^. position @i

setPosition :: forall i s a. HasPosition' i s a => a -> s -> s
setPosition = setVL (position @i)

instance  -- see Note [Changing type parameters]
  ( Generic s
  , ErrorUnless i s (0 <? i && i <=? Size (Rep s))
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
  , GHasPosition' i (Rep s) a
  , GHasPosition' i (Rep s') a'
  , GHasPosition i (Rep s) (Rep t) a b
  , t ~ Infer s a' b
  , GHasPosition' i (Rep t') b'
  , s ~ Infer t b' a
  ) => HasPosition i s t a b where

  position f s = ravel (repLens . gposition @i) f s

-- See Note [Uncluttering type signatures]
instance {-# OVERLAPPING #-} HasPosition f (Void1 a) (Void1 b) a b where
  position = undefined

type family ErrorUnless (i :: Nat) (s :: Type) (hasP :: Bool) :: Constraint where
  ErrorUnless i s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field at position "
        ':<>: 'ShowType i
        )

  ErrorUnless _ _ 'True
    = ()

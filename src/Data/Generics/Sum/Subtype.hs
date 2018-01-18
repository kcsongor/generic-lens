{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Subtype
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structural subtype relationships between sum types.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Subtype
  ( -- *Prisms
    --
    -- $setup
    AsSubtype (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Internal.Void
import Data.Generics.Sum.Internal.Subtype

import GHC.Generics (Generic (Rep))

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.Lens
-- >>> :{
-- data Animal
--   = Dog Dog
--   | Cat Name Age
--   | Duck Age
--   deriving (Generic, Show)
-- data FourLeggedAnimal
--   = Dog4 Dog
--   | Cat4 Name Age
--   deriving (Generic, Show)
-- data Dog = MkDog
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
-- dog4, cat4 :: FourLeggedAnimal
-- dog4 = Dog4 (MkDog "Snowy" 4)
-- cat4 = Cat4 "Garfield" 6
-- :}

-- |Structural subtyping between sums. A sum 'Sub' is a subtype of another sum
--  'Sup' if a value of 'Sub' can be given (modulo naming of constructors)
--  whenever a value of 'Sup' is expected. In the running example for instance,
--  'FourLeggedAnimal` is a subtype of 'Animal' since a value of the former can
--  be given as a value of the latter (renaming 'Dog4' to 'Dog' and 'Cat4' to
--  'Cat').
class AsSubtype sub sup where
  -- |A prism that captures structural subtyping. Allows a substructure to be
  --  injected (upcast) into a superstructure or a superstructure to be downcast
  --  into a substructure (which may fail).
  --
  --  >>> _Sub # dog4 :: Animal
  --  Dog (MkDog {name = "Snowy", age = 4})
  --
  --  >>> cat ^? _Sub :: Maybe FourLeggedAnimal
  --  Just (Cat4 "Mog" 5)
  --
  --  >>> duck ^? _Sub :: Maybe FourLeggedAnimal
  --  Nothing
  _Sub :: Prism' sup sub
  _Sub eta = prism injectSub projectSub eta
  {-# INLINE _Sub #-}

  -- |Injects a subtype into a supertype (upcast).
  injectSub  :: sub -> sup
  injectSub = build (_Sub @sub @sup)

  -- |Projects a subtype from a supertype (downcast).
  projectSub :: sup -> Either sup sub
  projectSub = withPrism (_Sub @sub @sup) (\_ b -> b)

  {-# MINIMAL (injectSub, projectSub) | _Sub #-}

instance
  ( Generic sub
  , Generic sup
  , GAsSubtype (Rep sub) (Rep sup)
  ) => AsSubtype sub sup where

  _Sub = repIso . _GSub . fromIso repIso

-- See Note [Uncluttering type signatures]
instance {-# OVERLAPPING #-} AsSubtype a Void where
  injectSub = undefined
  projectSub = undefined
instance {-# OVERLAPPING #-} AsSubtype Void a where
  injectSub = undefined
  projectSub = undefined

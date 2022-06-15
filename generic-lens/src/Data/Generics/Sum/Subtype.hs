{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Subtype
-- Copyright   :  (C) 2020 Csongor Kiss
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

import "this" Data.Generics.Internal.VL.Prism

import "generic-lens-core" Data.Generics.Internal.Void
import qualified "generic-lens-core" Data.Generics.Sum.Internal.Subtype as Core


-- $setup
-- == /Running example:/
--
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
  _Sub = prism injectSub (\i -> maybe (Left i) Right (projectSub i))
  {-# INLINE _Sub #-}

  -- |Injects a subtype into a supertype (upcast).
  injectSub  :: sub -> sup
  injectSub
    = build (_Sub @sub @sup)

  -- |Projects a subtype from a supertype (downcast).
  projectSub :: sup -> Maybe sub
  projectSub
    = either (const Nothing) Just . match (_Sub @sub @sup)

  {-# MINIMAL (injectSub, projectSub) | _Sub #-}

instance Core.Context sub sup => AsSubtype sub sup where
  _Sub f = prism2prismvl Core.derived f
  {-# INLINE _Sub #-}

-- | Reflexive case
--
--  >>> _Sub # dog :: Animal
--  Dog (MkDog {name = "Shep", age = 3})
instance {-# OVERLAPPING #-} AsSubtype a a where
  _Sub = id
  {-# INLINE _Sub #-}

-- | Uncluttering type signatures (see 'Void')
--
-- >>> :t +d _Sub
-- _Sub
--   :: (AsSubtype sub sup, Choice p, Applicative f) =>
--      p sub (f sub) -> p sup (f sup)
instance {-# OVERLAPPING #-} AsSubtype a Void where
  injectSub = undefined
  projectSub = undefined

-- | Uncluttering type signatures (see 'Void')
--
-- >>> :t +d _Sub @Int
-- _Sub @Int
--   :: (AsSubtype Int sup, Choice p, Applicative f) =>
--      p Int (f Int) -> p sup (f sup)
instance {-# OVERLAPPING #-} AsSubtype Void a where
  injectSub = undefined
  projectSub = undefined

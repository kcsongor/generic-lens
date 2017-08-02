{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
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
    --  $example
    AsSubtype (..)
  ) where

import Data.Generics.Internal.HList
import Data.Generics.Internal.Lens
import Data.Generics.Sum.Typed

import Data.Kind
import GHC.Generics

--  $example
--  @
--    module Example where
--
--    import Data.Generics.Sum
--    import GHC.Generics
--
--    data Animal
--      = Dog Dog
--      | Cat Name Age
--      | Duck Age
--      deriving (Generic, Show)
--
--    data FourLeggedAnimal
--      = Dog4 Dog
--      | Cat4 Name Age
--      deriving (Generic, Show)
--
--    data Dog = MkDog
--      { name :: Name
--      , age  :: Age
--      }
--      deriving (Generic, Show)
--
--    type Name = String
--    type Age  = Int
--
--    dog, cat, duck :: Animal
--
--    dog = Dog (MkDog "Shep" 3)
--    cat = Cat "Mog" 5
--    duck = Duck 2
--
--    dog4, cat4 :: FourLeggedAnimal
--
--    dog4 = Dog4 (MkDog "Snowy" 4)
--    cat4 = Cat4 "Garfield" 6
--  @

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
  --  >>> cat ^? _Sub :: Maybe FourLeggedAnimal
  --  Just (Cat4 "Mog" 5)
  --  >>> duck ^? _Sub :: Maybe FourLeggedAnimal
  --  Nothing
  _Sub :: Prism' sup sub
  _Sub = prism injectSub projectSub

  -- |Injects a subtype into a supertype (upcast).
  injectSub  :: sub -> sup

  -- |Projects a subtype from a supertype (downcast).
  projectSub :: sup -> Either sup sub

  {-# MINIMAL injectSub, projectSub #-}

instance
  ( Generic sub
  , Generic sup
  , GAsSubtype (Rep sub) (Rep sup)
  ) => AsSubtype sub sup where

  injectSub  = to . ginjectSub . from
  projectSub = either (Left . to) (Right . to) . gprojectSub . from

-- |As 'AsSubtype' but over generic representations as defined by
--  "GHC.Generics".
class GAsSubtype (subf :: Type -> Type) (supf :: Type -> Type) where
  ginjectSub  :: subf x -> supf x
  gprojectSub :: supf x -> Either (supf x) (subf x)

instance
  ( GAsSubtype l supf
  , GAsSubtype r supf
  ) => GAsSubtype (l :+: r) supf where

  ginjectSub x = case x of
    L1 l -> ginjectSub l
    R1 r -> ginjectSub r
  gprojectSub x
    = case gprojectSub x of
        Left  _ -> fmap R1 (gprojectSub x)
        Right y -> Right (L1 y)

instance
  ( GAsType supf a
  , GCollectible subf as
  , ListTuple a as
  ) => GAsSubtype (C1 meta subf) supf where

  ginjectSub
    = ginjectTyped . listToTuple . gtoCollection . unM1
  gprojectSub
    = fmap (M1 . gfromCollection . tupleToList) . gprojectTyped

instance GAsType supf a => GAsSubtype (S1 meta (Rec0 a)) supf where
  ginjectSub
    = ginjectTyped @supf . unK1 . unM1
  gprojectSub
    = fmap (M1 . K1) . gprojectTyped @supf

instance GAsSubtype subf supf => GAsSubtype (D1 meta subf) supf where
  ginjectSub
    = ginjectSub . unM1
  gprojectSub
    = fmap M1 . gprojectSub

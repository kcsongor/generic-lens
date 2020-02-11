{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE DuplicateRecordFields        #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE NoMonomorphismRestriction    #-}
{-# LANGUAGE OverloadedLabels             #-}
{-# LANGUAGE PartialTypeSignatures        #-}
{-# LANGUAGE Rank2Types                   #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# OPTIONS_GHC -Wno-missing-signatures   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports  #-}

module Examples where

import Data.Function ((&))
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics
import Data.Generics.Labels
import Data.Generics.Internal.VL.Iso
import Data.Generics.Internal.VL.Prism
import Data.Generics.Internal.Profunctor.Lens
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism

data Animal = Animal
  { name :: String
  , age  :: Int
  , eats :: String
  } deriving (Show, Generic)

data Human = Human
  { name    :: String
  , age     :: Int
  , address :: String
  , eats    :: String
  } deriving (Show, Generic)

data Living
  = Animal' { name :: String, eats :: String, age :: Int }
  | Human'  { name :: String, age :: Int, address :: String, eats :: String }
  deriving (Show, Generic)

toby :: Human
toby = Human { name = "Toby", age = 10, address = "London", eats = "Bread" }

growUp :: Animal -> Animal
growUp (Animal n a _) = Animal n (a + 10) "raw meat"

data MyRecord = MyRecord { field1 :: Int, field2 :: String } deriving Generic

--g :: Subtype s MyRecord => s -> String
--g s = s ^. super @MyRecord . label @"field2"

data Test a b = Test { fieldInt :: Int, fieldA :: a, fieldB :: b } deriving (Generic, Show)

-- | changedA :: Test Int String
-- >>> changedA
-- Test {fieldInt = 10, fieldA = 10, fieldB = "world"}
changedA = Test 10 "hello" "world" & field @"fieldA" .~ (10 :: Int)

-- | changedB :: Test String Int
-- >>> changedB
-- Test {fieldInt = 10, fieldA = "hello", fieldB = 10}
changedB = (Test 10 "hello" "world") & field @"fieldB" .~ (10 :: Int)

data Animal2 a
  = Dog (Dog a)
  | Cat Name Age
  | Duck Age
  deriving (Generic, Show)

data Dog a
  = MkDog
  { name   :: Name
  , age    :: Age
  , fieldA :: a
  }
  deriving (Generic, Show)
type Name = String
type Age  = Int
dog :: Animal2 Int
dog = Dog (MkDog "Shep" 3 30)

-- TODO: the error message for this case is ugly
-- data Dog a
--   = MkDog
--   { name    :: Name
--   , age     :: Age
--   , fieldA  :: a
--   , fieldA' :: a
--   }
--   deriving (Generic, Show)

-- |
-- >>> :t dog'
-- dog' :: Animal2 [Char]
dog' = dog & _Ctor @"Dog" . field @"fieldA" .~ "now it's a String"

stuff ::
  ( HasPosition 15 s t a String
  , HasField "test" s' t' a' b'
  , HasField "bar" a' b' s t
  ) => s' -> t'
stuff r = r & field @"test" . field @"bar" . position @15 .~ "hello"

stuff' ::
  ( HasPosition 15 s t a String
  , HasField "test" s' t' a' b'
  , HasField "bar" a' b' s t
  ) => s' -> t'
stuff' r = r & #test . #bar . position @15 .~ "hello"

data Foo m s = Foo
  { foo1 :: m s
  , foo2 :: [s]
  } deriving Generic

modifyFoo2 :: Foo (Either String) Int -> Foo Maybe Int
modifyFoo2 x = x & field @"foo1" .~ pure (1 :: Int)

data Bar a b = Bar
  { barField :: (a, b)
  } deriving Generic

modifiedBar = (Bar ("hello", "world")) & field @"barField" .~ ('c', 1 :: Int)

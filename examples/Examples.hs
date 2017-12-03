{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports  #-}

module Examples where

import Data.Function ((&))
import Data.Generics.Internal.Lens
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics

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

-- changedA :: Test Int String
-- >>> changedA
-- Test {fieldInt = 10, fieldA = 10, fieldB = "world"}
changedA = Test 10 "hello" "world" & field @"fieldA" .~ (10 :: Int)

-- changedB :: Test String Int
-- >>> changedB
-- Test {fieldInt = 10, fieldA = "hello", fieldB = 10}
changedB = (Test 10 "hello" "world") & field @"fieldB" .~ (10 :: Int)

--changedInt = set (field @"fieldInt") ("hello") (Test 10 "hello" "world")
-- type error

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
dog, cat, duck :: Animal2 Int
dog = Dog (MkDog "Shep" 3 30)
cat = Cat "Mog" 5
duck = Duck 2


--dog' :: Animal2 String
dog' = dog & _Ctor @"Dog" . field @"fieldA" .~ "now it's a String"

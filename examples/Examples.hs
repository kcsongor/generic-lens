{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports  #-}

module Examples where

import GHC.Generics
import Data.Generics.Product
--import Control.Lens

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

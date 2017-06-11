{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Examples where

import GHC.Generics
import Data.Generics.Record
import Data.Generics.Product
import Control.Lens

data Human = Human
  { name    :: String
  , age     :: Int
  , address :: String
  , eats    :: String
  } deriving (Show, Generic)

human :: Human
human = Human { name = "Tunyasz", age = 10, address = "London", eats = "Bread" }

data Animal = Animal
  { name :: String
  , age  :: Int
  , eats :: String
  } deriving (Show, Generic)

growUp :: Animal -> Animal
growUp (Animal name age _) = Animal name (age + 10) "raw meat"

data WildAnimal = WildAnimal
  { name  :: String
  , age   :: Int
  , eats  :: String
  , hunts :: Animal
  } deriving (Show, Generic)

raiseByWolves :: Human -> Human
raiseByWolves human = human & super %~ growUp

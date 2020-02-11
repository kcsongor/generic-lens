{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Biscuits where

import Optics.Core
import Data.Generics.Product
import GHC.Generics (Generic)

-- $setup
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> import Optics.Core
-- >>> import Data.Generics.Sum

data Item = Item
 { name :: String
 , cost :: Cost
 } deriving (Generic, Show)

newtype Cost = Cost Double deriving (Generic, Show)

data Invoice p = Invoice
 { item :: Item
 , name :: String
 , number :: Int
 , priority :: p
 } deriving (Generic, Show)

data Orders = Orders [ Invoice Int ] [ Invoice (Int, Double) ]
  deriving (Generic, Show)

-- |
-- >>> view (field @"name") bourbon
-- "Bourbon"
-- >>> bourbon & field @"cost" .~ Cost 110
-- Item {name = "Bourbon", cost = Cost 110.0}
--
-- >>> bourbon & field @"cost" %~ (\(Cost c) -> (Cost (c + 5)))
-- Item {name = "Bourbon", cost = Cost 105.0}
--
-- >>> Invoice bourbon "Johnny" 2 2 & field @"priority" %~ (\i -> (i, 0))
-- Invoice {item = Item {name = "Bourbon", cost = Cost 100.0}, name = "Johnny", number = 2, priority = (2,0)}
--
-- >>> view (field @"weight") bourbon
-- ...
-- ... The type Item does not contain a field named 'weight'.
-- ...
--
-- >>> bourbon & typed @Cost .~ Cost 200
-- Item {name = "Bourbon", cost = Cost 200.0}
--
-- >>> bourbon & typed %~ ("Chocolate " ++)
-- Item {name = "Chocolate Bourbon", cost = Cost 100.0}
--
-- >>> view (position @1) (42, "foo")
-- 42
-- >>> view (position @1) (42, "foo", False)
-- 42
-- >>> view (position @2) orders
-- [Invoice {item = Item {name = "Bourbon", cost = Cost 100.0}, name = "George", number = 2, priority = (0,3.0)}]
--
-- >>> view (position @2) orders
-- [Invoice {item = Item {name = "Bourbon", cost = Cost 100.0}, name = "George", number = 2, priority = (0,3.0)}]
--
-- >>> view (position @3) orders
-- ...
-- ... The type Orders does not contain a field at position 3
-- ...
--
-- >>> view (super @Item) (WItem "Bourbon" (Cost 2000) (Weight 0.03))
-- Item {name = "Bourbon", cost = Cost 2000.0}
--
-- >>> (WItem "Bourbon+" (Cost 500) (Weight 0.03)) & super @Item .~ bourbon
-- WItem {name = "Bourbon", cost = Cost 100.0, weight = Weight 3.0e-2}
--
-- >>> DInt 1 ^? _Ctor @"DInt"
-- Just 1
--
-- >>> _Typed # (False, "wurble") :: D
-- DPair False "wurble"
--
-- >>>  EChar 'a' ^? _Sub @D
-- Nothing
--
-- >>> _Sub  # DInt 10 :: E
-- EInt 10
bourbon :: Item
bourbon = Item "Bourbon" (Cost 100)

orders :: Orders
orders = Orders  [Invoice bourbon "Earl" 1 0 , Invoice bourbon "Johnny" 2 2]
                 [Invoice bourbon "George" 2 (0, 3)]

nameOfItem :: Invoice p -> String
nameOfItem = view (field @"item" % field @"name")

thankYou :: Orders -> Orders
thankYou = over (types @Cost) (\(Cost c) -> Cost (c * 0.85))

thankYouPriority :: Orders -> Orders
thankYouPriority = over (position @2 % types @Cost) (\(Cost c) -> Cost (c * 0.85))

upgrade :: Double -> Invoice Int -> Invoice (Int, Double)
upgrade bribe invoice = over (param @0) (\i -> (i, bribe)) invoice

audit :: Orders -> [Item]
audit = toListOf (types @Item)

newtype Weight = Weight Double deriving (Generic, Show)
data WeighedItem = WItem
 { name :: String
 , cost :: Cost
 , weight :: Weight
 } deriving (Generic, Show)

data D = DInt Int | DPair Bool String
  deriving (Generic, Show)

data E = EInt Int | EPair Bool String | EChar Char
  deriving (Generic, Show)

costInc :: HasTypes t Cost => t -> t
costInc = over (types @Cost) (\(Cost c) -> Cost (c + 5))

modifyPriority :: (Int -> Int) -> Invoice Int -> Invoice Int
modifyPriority = over (types @Int)

treeIncParam :: HasParam 0 s s Int Int => s -> s
treeIncParam = over (param @0) (+ 1)

instance Functor Invoice where
  fmap = over (param @0)

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-
Example adapted from SYB
========================
-}

module Main (main) where

import Control.Lens.Fold (toListOf)
import Control.Monad (void)
import Data.Generics.Product
import GHC.Generics
import Test.HUnit

main :: IO ()
main = void $ runTestTT tests

-- A parameterised datatype for binary trees with data at the leafs
data Tree a w = Leaf a
              | Fork (Tree a w) (Tree a w)
              | WithWeight (Tree a w) w
       deriving (Show, Generic)

-- A typical tree
mytree :: Tree Int Int
mytree = Fork (WithWeight (Leaf 42) 1)
              (WithWeight (Fork (Leaf 88) (Leaf 37)) 2)

-- Print everything like an Int in mytree
-- In fact, we show two attempts:
--   1. print really just everything like an Int
--   2. print everything wrapped with Leaf
-- So (1.) confuses leafs and weights whereas (2.) does not.
tests :: Test
tests = show ( toListOf (typesDeep @Int) mytree
             , toListOf (param @1) mytree
             ) ~=? output

-- original code from SYB:
--tests = show ( listify (\(_::Int) -> True)         mytree
--             , everything (++) ([] `mkQ` fromLeaf) mytree
--             ) ~=? output
--  where
--    fromLeaf :: Tree Int Int -> [Int]
--    fromLeaf (Leaf x) = [x]
--    fromLeaf _        = []

output :: String
output = "([42,1,88,37,2],[42,88,37])"

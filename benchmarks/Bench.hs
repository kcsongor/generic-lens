{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.DeepSeq
import Control.Lens.Operators
import Control.Lens.Type
import Control.Monad
import Criterion.Main
import Data.Generics.Product
import GHC.Generics
import Test.QuickCheck

main :: IO ()
main = defaultMain
  [ env (arbitraryAnimalsOfLength 100) $ products 100
  , env (arbitraryAnimalsOfLength 1000) $ products 1000
  , env (arbitraryAnimalsOfLength 10000) $ products 10000
  , env (arbitraryAnimalsOfLength 100000) $ products 100000
  ]

products :: Int -> [Animal] -> Benchmark
products n as
  = bgroup ("products/" ++ show n)
      [ bench "generic-lens/get" (nf (const $ map (^. field @"name") as) ())
      , bench "lens/get" (nf (const $ map (^. aName) as) ())
      , bench "generic-lens/set" (nf (const $ map (\a -> a & field @"name" .~ "Name") as) ())
      , bench "lens/set" (nf (const $ map (\a -> a & aName .~ "Name") as) ())
      ]

arbitraryAnimalsOfLength :: Int -> IO [Animal]
arbitraryAnimalsOfLength n
  = replicateM n (generate arbitrary)

data Animal = Animal
  { name :: String
  , age  :: Int
  , eats :: String
  } deriving (Generic, Show)

instance Arbitrary Animal where
  arbitrary = Animal <$> arbitrary <*> arbitrary <*> arbitrary

instance NFData Animal where
  rnf Animal{..} = rnf name `seq` rnf age `seq` rnf eats

aName :: Lens' Animal String
aName f Animal{..}
  = (\x -> Animal { name = x, age, eats }) <$> f name

{-
aAge :: Lens' Animal Int
aAge f Animal{..}
  = (\x -> Animal { name, age = x, eats }) <$> f age

aEats :: Lens' Animal String
aEats f Animal{..}
  = (\x -> Animal { name, age, eats = x }) <$> f eats
  -}

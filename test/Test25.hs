{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

module Test25 where

import Control.Lens
import Data.Generics.Product
import GHC.Generics

data Record1 = Record1
    { field1 :: Int
    , field2 :: Double
    } deriving (Generic)

class Default a where
    def :: a

instance Default Record1 where
    def = Record1 0 0.0

f :: Record1 -> Int
f r = r ^. field @"field1"

main :: IO ()
main = do
    print $ f def
    print $ f ( field @"field1" .~ 1 $ (def :: Record1))
    print $ f ( field @"field1" .~ 2 $ Record1 0 0.0)
    print $ f ( field @"field1" .~ (1 :: Int) $ def)
    print $ f ( position @1 .~ (1 :: Int) $ def)

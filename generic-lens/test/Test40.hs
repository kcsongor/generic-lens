{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

module Test40 where

import Data.Generics.Product
import GHC.Generics

class MyClass a where
  data AssocData a

instance MyClass Int where
  data AssocData Int = SomeData
    { val :: Int
    } deriving (Generic)

main :: IO ()
main
  = print $ getField @"val" (SomeData 3)

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

module TestX where

import Data.Generics.Product
import GHC.Generics
import Control.Lens
import Data.HashMap.Monoidal as HM

newtype Res a = Res {
  hm :: HM.MonoidalHashMap a String
} deriving (Eq, Show)

main :: IO ()
main = do
  let foo = Res (HM.fromList [(1, "foo"), (2, "bar")])
  print $ foo ^.. (field @"hm").traverse

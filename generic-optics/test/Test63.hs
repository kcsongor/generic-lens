{-# LANGUAGE DataKinds, DeriveGeneric, TypeApplications #-}
module Test63 (example) where
import Data.Generics.Product (types)
import Optics.Core
import Data.Word (Word32)
import GHC.Generics (Generic)

data Record = Record {field1 :: Word32, field2 :: Int}
    deriving (Generic, Show)

example :: Record
example = over (types @Int) (+1) (Record 0 0)

{-# LANGUAGE DataKinds, DeriveGeneric, TypeApplications #-}
module Test62 (example) where
import Data.Generics.Product (field)
import Data.Generics.Internal.VL.Lens (set)
import GHC.Generics (Generic)

data Foo a = Foo { bar :: Bar a } deriving Generic
data Bar a = Bar { x :: a, y :: a } deriving Generic

example :: Foo ()
example =
  set (field @"bar" . field @"x") ()
  . set (field @"bar" . field @"y") ()
  $ Foo{ bar = Bar{ x = (), y = () } }

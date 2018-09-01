{-# LANGUAGE DataKinds, DeriveGeneric, TypeApplications #-}
module Test62 (example) where
import Data.Generics.Product (field, position)
import Data.Generics.Internal.VL.Lens (set)
import GHC.Generics (Generic)

data Foo a = Foo { bar :: Bar a } deriving Generic
data Bar a = Bar { x :: a, y :: a } deriving Generic

example :: Foo ()
example =
  set (field @"bar" . position @1) ()
  . set (position @1 . field @"y") ()
  $ Foo{ bar = Bar{ x = (), y = () } }

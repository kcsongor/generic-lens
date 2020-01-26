{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | 

module Test88 where

import Optics.Core
import Data.Generics.Product.Param
import GHC.Generics

data Foo a = Foo a deriving (Eq, Show, Generic)
data Bar b = Bar b deriving (Eq, Show, Generic)
data FooBar c = FooBar (Foo (Bar c)) deriving (Eq, Show, Generic)

foo :: FooBar Int -> FooBar String
foo = over (param @0) show

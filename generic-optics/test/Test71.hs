{-# LANGUAGE FlexibleContexts, DeriveGeneric, TypeApplications #-}

module Test71 where

import GHC.Generics
import Optics.Core
import Data.Generics.Product

data Foobar = Foobar Int Char String
    deriving Generic

foo :: HasType Foobar ctx => ctx -> Char
foo ctx = case view (typed @Foobar) ctx of
    Foobar _ c _ -> c

bar :: Char
bar = foo (Foobar 3 'a' "Hello")

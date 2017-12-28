{-# LANGUAGE DataKinds, DeriveGeneric, TypeApplications #-}

module Main where

-- Test case from #24, comments preserved
import Control.Lens
import Data.Generics.Product.Fields
import Data.Generics.Product.Positions
import GHC.Generics

data Foo a b = Foo { x1 :: a, x2 :: b } deriving (Generic, Show)

data Bar a b = Bar { x3 :: Foo a b, x4 :: Int } deriving (Generic, Show)

tup :: ((Int, Char), Int)
tup = ((1, 'a'), 2)
tup2, tup3, tup4 :: ((Char, Char), Int)
tup2 = tup & _1 . _1 %~ toEnum  -- Works.
tup3 = tup & x %~ toEnum  -- Works also with type annotation.
  where x :: Lens ((Int, Char), Int) ((Char, Char), Int) Int Char
        x = _1 . _1
-- Works.
tup4 = tup & position @1 . position @1 %~ toEnum

foo :: Foo Int Char
foo = Foo 1 'a'
foo2, foo3 :: Foo Char Char
foo2 = foo & field @"x1" %~ toEnum  -- Works when there's just one 'field'.
foo3 = foo & position @1 %~ toEnum -- Works when there's just one 'position'.

bar :: Bar Int Char
bar = Bar (Foo 1 'a') 2
bar2, bar3, bar4 :: Bar Char Char
-- Doesn't work, error at first 'field' (Couldn't match type ‘Int’ with ‘Char’ arising from a use of ‘field’).
bar2 = bar & field @"x3" . field @"x1" %~ toEnum
-- Type annotation doesn't help.
bar3 = bar & l %~ toEnum
  where l :: Lens (Bar Int Char) (Bar Char Char) Int Char
        l = field @"x3" . field @"x1"
-- Doesn't work, error at first 'position' (Couldn't match type ‘Int’ with ‘Char’ arising from a use of ‘position’).
bar4 = bar & position @1 . position @1 %~ toEnum
-- Works if we stick to simple Lens' (modify to the same type).
bar5 :: Bar Int Char
bar5 = bar & field @"x3" . field @"x1" %~ (+1)

main :: IO ()
main = print bar5

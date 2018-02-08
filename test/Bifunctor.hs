{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import Control.Lens hiding (Bifunctor(..))
import           Control.Monad (void)
import Data.Generics.Product
import GHC.Generics
import Test.HUnit

main :: IO ()
main = void $ runTestTT $
  bimap (* 2) show mytree ~=? mytreeBimapped

data Tree a w = Leaf a
              | Fork (Tree a w) (Tree a w)
              | WithWeight (Tree a w) w
       deriving (Show, Eq, Generic, Bifunctor)

mytree :: Tree Int Int
mytree = Fork (WithWeight (Leaf 42) 1)
              (WithWeight (Fork (Leaf 88) (Leaf 37)) 2)

mytreeBimapped :: Tree Int String
mytreeBimapped = Fork (WithWeight (Leaf 84) "1")
                      (WithWeight (Fork (Leaf 176) (Leaf 74)) "2")

-- Derivable Bifunctor
class Bifunctor p where
  bimap :: (a -> c) -> (b -> d) -> p a b -> p c d

  default bimap ::
    ( HasParam 0 (p a b) (p a d) b d
    , HasParam 1 (p a d) (p c d) a c
    ) => (a -> c) -> (b -> d) -> p a b -> p c d
  bimap f g s = s & param @0 %~ g & param @1 %~ f

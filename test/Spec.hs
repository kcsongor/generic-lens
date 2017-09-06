{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}

{-# LANGUAGE AllowAmbiguousTypes             #-}
{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE DeriveGeneric                   #-}
{-# LANGUAGE DuplicateRecordFields           #-}
{-# LANGUAGE ExistentialQuantification       #-}
{-# LANGUAGE RankNTypes                      #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeApplications                #-}

module Main where

import GHC.Generics
import Data.Generics.Product
import GHC.Proof

main :: IO ()
main = putStrLn "Hello world"

data Record = MkRecord
  { fieldA :: Int
  , fieldB :: Bool
  } deriving Generic

data Record2 = MkRecord2
  { fieldA :: Int
  } deriving Generic

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

fieldALensManual :: Lens' Record Int
fieldALensManual f (MkRecord a b) = (\a' -> MkRecord a' b) <$> f a

newtype L s a = L (Lens' s a)

subtypeLensManual :: Lens' Record Record2
subtypeLensManual f record
  = fmap (\ds -> case record of
                  MkRecord _ b -> MkRecord (case ds of {MkRecord2 g1 -> g1}) b
         ) (f (MkRecord2 (case record of {MkRecord a _ -> a})))

--------------------------------------------------------------------------------
-- * Tests
-- The ghc-proofs plugin checks that the following equalities hold, by checking
-- that the LHSs and the RHSs are CSEd. This also means that the runtime
-- characteristics of the derived lenses is the same as the manually written
-- ones above.

fieldP :: Proof
fieldP = L fieldALensManual === L (field @"fieldA")

posP :: Proof
posP = L fieldALensManual === L (position @1)

subtypeP :: Proof
subtypeP = L subtypeLensManual === L super

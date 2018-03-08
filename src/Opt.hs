{-# OPTIONS_GHC -dsuppress-all #-}
{-# OPTIONS_GHC -ddump-simpl #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
-- Like this for the HsModule example, would be fine with INLINE pragmas on
-- the generic methods
{-# OPTIONS_GHC -funfolding-use-threshold=100000 #-}
module Opt where

import Data.Generics.Product
import Data.Generics.Internal.VL.Lens

import GHC.Generics
import GHC.TypeLits
import Data.Type.Bool hiding (type (||))

data FlatProd =
  FlatProd Int Char Bool Int Char [Int] [Bool] (Either Char (Maybe Char)) String String String
  deriving (Generic, Show)

testFlatProd
  = FlatProd 10 'c' False 20 'c' [1,2,3] [True, False] (Left 'c') "hello" "world" "foo"

updateFlatProdInt :: FlatProd -> FlatProd
updateFlatProdInt = (typesDeep @Int) .~ 10000


{-
 Some notes:
 ~~~~~~~~~~~

The optimiser takes care of eliminating traversals into deeper structure
_as long as_ they are not recursive.

data FlatProd =
  FlatProd Int Char Bool Int Char [Int] [Bool] (Either Char (Maybe Char)) String String String
  deriving (Generic)

updateFlatProdInt
updateFlatProdInt
  = \ _ eta_B1 ->
      case eta_B1 of
      { FlatProd g1_a6mr g2_a6ms g3_a6mt g4_a6mu g5_a6mv g6_a6mw g7_a6mx
                 g8_a6my g9_a6mz g10_a6mA g11_a6mB ->
      FlatProd
        updateFlatProdInt_b1 -- Int
        g2_a6ms -- Char
        g3_a6mt -- Bool
        updateFlatProdInt_b1 -- Int
        g5_a6mv -- Char
        ((updateFlatProdInt_$s$dHasTypesDeep2
            (updateFlatProdInt1 `cast` <Co:5>) g6_a6mw)
         `cast` <Co:3>) -- [Int]
        (($w$s$dHasTypesDeep1 g7_a6mx) `cast` <Co:3>) -- [Bool]
        g8_a6my <- Either Char Bool
        ~~~~~~~
        (($w$s$dHasTypesDeep g9_a6mz) `cast` <Co:2>) -- String
        (($w$s$dHasTypesDeep g10_a6mA) `cast` <Co:2>) -- String
        (($w$s$dHasTypesDeep g11_a6mB) `cast` <Co:2>) -- String
      }

The traversal into `Either Char Bool` is eliminated, but not `String`s and `[Bool]`s...

+----------------------------------+
| data String = "" | Char : String |
+----------------------------------+

certainly contains no Ints...

The @Int traversal for String, however:

Rec {
-- RHS size: {terms: 10, types: 9, coercions: 11, joins: 0/0}
$w$s$dHasTypesDeep
$w$s$dHasTypesDeep
  = \ w_s6Uf ->
      case w_s6Uf of {
        [] -> [] `cast` <Co:4>;
        : g1_i6JG g2_i6JH ->
          (: g1_i6JG (($w$s$dHasTypesDeep g2_i6JH) `cast` <Co:3>))
          `cast` <Co:4>
      }
end Rec }

The generated dictionary is recursive.

We know for lists that if nothing interesting happens in one step, nothing
interesting will happen at all.

But in general, how many steps are needed?

-}

data Poly a b = PNil | PCons a (Poly b a) deriving (Generic, Show)

updatePoly :: Poly Int String -> Poly Int String
updatePoly = typesDeep @Int .~ 10

{-

Poly is polymorphically recursive, which means that we actually need 2 steps to
find out if there's anything interesting there.

A more complicated example:
-}

type family Fib n where
  Fib 0 = 1
  Fib 1 = 2
  Fib n = Fib (n - 1) + Fib (n - 2)

data FList (n :: Nat)
  = FNil
  | (If (n <=? Fib 2) String Int) :> (FList (If (n <=? Fib 2) (n + 1) n))
  deriving (Generic)

infixr 5 :>

test :: FList 0
test = "this" :> "won't" :> "end" :> "well" :> 10 :> FNil

updateFlist :: FList 0 -> FList 0
updateFlist = (typesDeep @Int) .~ 10

-- We can even have a deeply nested mutually recursive structure:
data T1 = T1L T2 | T1R T4 deriving Generic
data T2 = T2 T3 deriving Generic
data T3 = T3 T2 deriving Generic
data T4 = T4L T5 | T4R T6 deriving Generic
data T5 = T5 Int deriving Generic
data T6 = T6 T7 deriving Generic
data T7 = T7 T5 T1 deriving Generic

updateT1T2 :: T1 -> T1
updateT1T2 = typesDeep @Int .~ undefined

{-
  How do we know how many layers we need to look down?

  We can think of types as directed cyclic graphs.

  The graph of T1 can be represented with this tree:

                      T1
                   ↙      ↘
               ⤹ T2         T4
              T3  ⤴      ↙      ↘
                      T5 <---+    T6
                     ↙       \      ↘
                  Int         +------ T7


  Suppose we're looking for `Int`s. Where do we need to look?
  T2 and T3 are obviously not interesting, but we can see that T5 is. This
  means that T4 is as well, since we can get to T5 from there. But T7 can also
  lead to an Int, via T5. Which means T6 too...

  If left on its own, GHC doens't try to be clever with recursive cases like
  this, and the naive implementation of traversals will do extra work for
  recursive structures. For example, how do we know if T2 contains an Int? Well,
  we look inside T3, and see if that contains Ints. How do we know that? We look inside
  T2 of course! The generated dictionary will be recursive, and will
  fruitlessly look through a long chain of alternating T2s and T3s.

  We want to help GHC a bit. We do this by statically determining whether a
  recursive structure is interesting or not.

  First, we unroll the graph to remove cycles.

                      T1
                   ↙      ↘
                 T2         T4
              ↙         ↙      ↘
            T3        T5          T6
          ↙          ↙              ↘
        T2        Int                 T7
                                      ↓
                                      T5
                                      ↓
                                      Int

  Notice how T2 is not expanded further: we only unroll one level.

  This new tree has a nice property: if we do a DFS for `Int`, we will
  encounter all the nodes that we care about. Namely: T1, T4, T5, T6, T7.

  Then the traversal is guided as follows: we walk the tree, and if we
  encounter a node that doesn't appear in our collected list, we just skip it.


                      T1
                   ↙      ↘
               Stop         T4
                        ↙      ↘
                      T5          T6
                     ↙              ↘
                  Int                 T7
                                      ↓
                                      T5
                                      ↓
                                      Int

-}

data Mut a = Weight Int (Mut a) | Mut (Recu a) (Recu a) deriving Generic

data Recu a = Leaf a | Recu (Mut a) deriving Generic

-- mutRecT :: Mut Int -> Mut Int
-- mutRecT = typesDeep @Int .~ 10

-- TODO: typesDeep @String ....
-- TODO: run this to a fixed point


{-

Memoisation:

Before,

*Opt> :kind! DeepN FlatProd
DeepN FlatProd :: k -> *
= Node
    FlatProd
    (((Primitive Int :*: Primitive Char)
      :*: (Primitive Bool :*: (Primitive Int :*: Primitive Char)))
     :*: ((Node [Int] (U1 :+: (Primitive Int :*: K1 R [Int]))
           :*: (Node [Bool] (U1 :+: (Primitive Bool :*: K1 R [Bool]))
                :*: Node
                      (Either Char (Maybe Char))
                      (Primitive Char :+: Node (Maybe Char) (U1 :+: Primitive Char))))
          :*: (Node [Char] (U1 :+: (Primitive Char :*: K1 R [Char]))
               :*: (Node [Char] (U1 :+: (Primitive Char :*: K1 R [Char]))
                    :*: Node [Char] (U1 :+: (Primitive Char :*: K1 R [Char]))))))

After,

*Opt> :kind! DeepN FlatProd
DeepN FlatProd :: k -> *
= Node
    FlatProd
    (((Primitive Int :*: Primitive Char)
      :*: (Primitive Bool :*: (Primitive Int :*: Primitive Char)))
     :*: ((Node [Int] (U1 :*: (Primitive Int :*: K1 R [Int]))
           :*: (Node [Bool] (U1 :*: (Primitive Bool :*: K1 R [Bool]))
                :*: Node
                      (Either Char (Maybe Char))
                      (Primitive Char :*: Node (Maybe Char) (U1 :*: Primitive Char))))
          :*: (Node [Char] (U1 :*: (Primitive Char :*: K1 R [Char]))
               :*: (K1 R [Char] :*: K1 R [Char]))))
-}

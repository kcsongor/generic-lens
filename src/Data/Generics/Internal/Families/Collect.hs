{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.Families.Collect
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Families.Collect
  ( CollectTotalType
  , CollectPartialType
  , CollectField
  , CollectFieldsOrdered
  , TypeStat (..)
  , type (\\)
  ) where

import Data.Type.Bool     (If)
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits       (Symbol, CmpSymbol)

import Data.Generics.Product.Internal.List (type (++))
import Data.Generics.Internal.Families.Has (GTypes)

data TypeStat
  = TypeStat
    { _containsNone :: [Symbol]
    , _containsMultiple :: [Symbol]
    , _containsOne :: [Symbol]
    }

type EmptyStat = 'TypeStat '[] '[] '[]

type family CollectTotalType t f :: TypeStat where
  CollectTotalType t (C1 ('MetaCons ctor _ _) f)
    = AddToStat ctor (CountType t f) EmptyStat
  CollectTotalType t (M1 _ _ r)
    = CollectTotalType t r
  CollectTotalType t (l :+: r)
    = MergeStat (CollectTotalType t l) (CollectTotalType t r)

type family CollectField t f :: TypeStat where
  CollectField t (C1 ('MetaCons ctor _ _) f)
    = AddToStat ctor (CountField t f) EmptyStat
  CollectField t (M1 _ _ r)
    = CollectField t r
  CollectField t (l :+: r)
    = MergeStat (CollectField t l) (CollectField t r)

type family AddToStat (ctor :: Symbol) (count :: Count) (st :: TypeStat) :: TypeStat where
  AddToStat ctor 'None ('TypeStat n m o)     = 'TypeStat (ctor ': n) m o
  AddToStat ctor 'Multiple ('TypeStat n m o) = 'TypeStat n (ctor ': m) o
  AddToStat ctor 'One ('TypeStat n m o)      = 'TypeStat n m (ctor ': o)

type family MergeStat (st1 :: TypeStat) (st2 :: TypeStat) :: TypeStat where
  MergeStat ('TypeStat n m o) ('TypeStat n' m' o') = 'TypeStat (n ++ n') (m ++ m') (o ++ o')

type family CountType t f :: Count where
  CountType t (S1 _ (Rec0 t))
    = 'One
  CountType t (l :*: r)
    = CountType t l <|> CountType t r
  CountType t _
    = 'None

type family CountField (field :: Symbol) f :: Count where
  CountField field (S1 ('MetaSel ('Just field) _ _ _) _)
    = 'One
  CountField field (l :*: r)
    = CountField field l <|> CountField field r
  CountField _ _
    = 'None

type family CollectPartialType t f :: [Symbol] where
  CollectPartialType t (l :+: r)
    = CollectPartialType t l ++ CollectPartialType t r
  CollectPartialType t (C1 ('MetaCons ctor _ _) f)
    = If (t == GTypes f) '[ctor] '[]
  CollectPartialType t (D1 _ f)
    = CollectPartialType t f

data Count
  = None
  | One
  | Multiple

type family (a :: Count) <|> (b :: Count) :: Count where
  'None <|> b     = b
  a     <|> 'None = a
  a     <|> b     = 'Multiple

type family (a :: Count) <&> (b :: Count) :: Count where
  a <&> a = a
  _ <&> _ = 'Multiple

type family CollectFieldsOrdered (r :: * -> *) :: [Symbol] where
  CollectFieldsOrdered (l :*: r)
    = Merge (CollectFieldsOrdered l) (CollectFieldsOrdered r)
  CollectFieldsOrdered (S1 ('MetaSel ('Just name) _ _ _) _)
    = '[name]
  CollectFieldsOrdered (M1 _ m a)
    = CollectFieldsOrdered a
  CollectFieldsOrdered _
    = '[]

type family Merge (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  Merge xs '[] = xs
  Merge '[] ys = ys
  Merge (x ': xs) (y ': ys) = Merge' (CmpSymbol x y) x y xs ys

type family Merge' (ord :: Ordering) (x :: Symbol) (y :: Symbol) (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  Merge' 'LT x y xs ys = x ': Merge xs (y ': ys)
  Merge' _ x y xs ys   = y ': Merge (x ': xs) ys

type family (xs :: [Symbol]) \\ (ys :: [Symbol]) :: [Symbol] where
  xs \\ '[] = xs
  '[] \\ xs = '[]
  (x ': xs) \\ (y ': ys) = Sub' (CmpSymbol x y) x y xs ys

infixr 5 \\
type family Sub' (ord :: Ordering) (x :: Symbol) (y :: Symbol) (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  Sub' 'LT x y xs ys = x ': (xs \\ y ': ys)
  Sub' 'GT x _ xs ys = (x ': xs) \\ ys
  Sub' 'EQ _ _ xs ys = xs \\ ys

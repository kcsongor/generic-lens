{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generics.Internal.Families.Count
  ( CountType
  , Counted (..)
  ) where

import GHC.Generics
import GHC.TypeLits

type family CountType t f :: Counted where
  CountType t (S1 _ (Rec0 t))
    = 'One
  CountType t (l :*: r)
    = CountType t l <|> CountType t r
  CountType t (l :+: r)
    = CountType t l <&> CountType t r
  CountType t (S1 _ _)
    = 'None
  CountType t (C1 _ f)
    = CountType t f
  CountType t (D1 _ f)
    = CountType t f
  CountType t (Rec0 _)
    = 'None
  CountType t U1
    = 'None
  CountType t V1
    = 'None
  CountType t f
    = TypeError
        (     'ShowType f
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

data Counted
  = None
  | One
  | Multiple

type family (a :: Counted) <|> (b :: Counted) :: Counted where
  'None <|> b     = b
  a     <|> 'None = a
  a     <|> b     = 'Multiple

type family (a :: Counted) <&> (b :: Counted) :: Counted where
  a <&> a = a
  _ <&> _ = 'Multiple

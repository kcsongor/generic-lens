{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Data.Generics.Product.Internal.Param
  ( Context
  , derived
  ) where

import Data.Generics.Product.Internal.Types
import Data.Generics.Internal.VL.Traversal

import GHC.Generics
import Data.Kind
import Data.Generics.Internal.Families
import Data.Generics.Internal.GenericN
import Data.Generics.Internal.Errors
import GHC.TypeLits

type Context n s t a b
  =  ( GenericN s
     , GenericN t
     -- TODO: merge the old 'Changing' code with 'GenericN'
     , Defined (Rep s)
       (NoGeneric s
         '[ 'Text "arising from a generic traversal of the type parameter at position " ':<>: QuoteType n
          , 'Text "of type " ':<>: QuoteType a ':<>: 'Text " in " ':<>: QuoteType s
          ])
       (() :: Constraint)
     , s ~ Infer t (P n b 'PTag) a
     , t ~ Infer s (P n a 'PTag) b
     , Error ((ArgCount s) <=? n) n (ArgCount s) s
     , a ~ ArgAt s n
     , b ~ ArgAt t n
     , GHasTypes ChGeneric (RepN s) (RepN t) (Param n a) (Param n b)
     )

derived :: forall n s t a b. Context n s t a b => Traversal s t a b
derived = repIsoN . gtypes_ @ChGeneric . paramIso @n

-- this could be an iso but since we're operating on a VL traversal it's easier this way.
repIsoN :: (GenericN a, GenericN b) => Traversal a b (RepN a x) (RepN b x)
repIsoN f a = toN <$> f (fromN a)

-- this could be an iso but since we're operating on a VL traversal it's easier this way.
paramIso :: Traversal (Param n a) (Param n b) a b
paramIso f a = StarParam <$> f (getStarParam a)

type family Error (b :: Bool) (expected :: Nat) (actual :: Nat) (s :: Type) :: Constraint where
  Error 'False _ _ _
    = ()

  Error 'True expected actual typ
    = TypeError
        (     'Text "Expected a type with at least "
        ':<>: 'ShowType (expected + 1)
        ':<>: 'Text " parameters, but "
        ':$$: 'ShowType typ
        ':<>: 'Text " only has "
        ':<>: 'ShowType actual
        )

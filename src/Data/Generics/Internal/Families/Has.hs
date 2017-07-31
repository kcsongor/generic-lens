{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generics.Internal.Families.Has
  ( HasTotalFieldP
  , HasTotalTypeP
  , HasPartialTypeP
  , HasCtorP
  ) where

import Data.Generics.Internal.Families.Bool

import GHC.Generics
import GHC.TypeLits

type family HasTotalFieldP (field :: Symbol) f :: Bool where
  HasTotalFieldP field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'True
  HasTotalFieldP field (l :*: r)
    = HasTotalFieldP field l || HasTotalFieldP field r
  HasTotalFieldP field (l :+: r)
    = HasTotalFieldP field l && HasTotalFieldP field r
  HasTotalFieldP field (S1 _ _)
    = 'False
  HasTotalFieldP field (C1 _ f)
    = HasTotalFieldP field f
  HasTotalFieldP field (D1 _ f)
    = HasTotalFieldP field f
  HasTotalFieldP field (Rec0 _)
    = 'False
  HasTotalFieldP field U1
    = 'False
  HasTotalFieldP field V1
    = 'False
  HasTotalFieldP field f
    = TypeError
        (     'ShowType f
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

type family HasTotalTypeP a f :: Bool where
  HasTotalTypeP t (S1 meta (Rec0 t))
    = 'True
  HasTotalTypeP t (l :*: r)
    = HasTotalTypeP t l || HasTotalTypeP t r
  HasTotalTypeP t (l :+: r)
    = HasTotalTypeP t l && HasTotalTypeP t r
  HasTotalTypeP t (S1 _ _)
    = 'False
  HasTotalTypeP t (C1 m f)
    = HasTotalTypeP t f
  HasTotalTypeP t (D1 m f)
    = HasTotalTypeP t f
  HasTotalTypeP t (Rec0 _)
    = 'False
  HasTotalTypeP t U1
    = 'False
  HasTotalTypeP t V1
    = 'False
  HasTotalTypeP t f
    = TypeError
        (     'ShowType f
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

type family HasPartialTypeP a f :: Bool where
  HasPartialTypeP t (S1 meta (Rec0 t))
    = 'True
  HasPartialTypeP t (l :*: r)
    = HasPartialTypeP t l || HasPartialTypeP t r
  HasPartialTypeP t (l :+: r)
    = HasPartialTypeP t l || HasPartialTypeP t r
  HasPartialTypeP t (S1 _ _)
    = 'False
  HasPartialTypeP t (C1 m f)
    = HasPartialTypeP t f
  HasPartialTypeP t (D1 m f)
    = HasPartialTypeP t f
  HasPartialTypeP t (Rec0 _)
    = 'False
  HasPartialTypeP t U1
    = 'False
  HasPartialTypeP t V1
    = 'False
  HasPartialTypeP t f
    = TypeError
        (     'ShowType f
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

type family HasCtorP (ctor :: Symbol) f :: Bool where
  HasCtorP ctor (C1 ('MetaCons ctor _ _) _)
    = 'True
  HasCtorP ctor (f :+: g)
    = HasCtorP ctor f || HasCtorP ctor g
  HasCtorP ctor (D1 m f)
    = HasCtorP ctor f
  HasCtorP ctor _
    = 'False

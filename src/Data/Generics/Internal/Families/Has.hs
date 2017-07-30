{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generics.Internal.Families.Has
  ( HasFieldP
  , HasTypeP
  ) where

import GHC.Generics
import GHC.TypeLits

type family HasFieldP (field :: Symbol) f :: Bool where
  HasFieldP field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'True
  HasFieldP field (l :*: r)
    = HasFieldP field l || HasFieldP field r
  HasFieldP field (l :+: r)
    = HasFieldP field l && HasFieldP field r
  HasFieldP field (S1 _ _)
    = 'False
  HasFieldP field (C1 _ f)
    = HasFieldP field f
  HasFieldP field (D1 _ f)
    = HasFieldP field f
  HasFieldP field (Rec0 _)
    = 'False
  HasFieldP field U1
    = 'False
  HasFieldP field V1
    = 'False
  HasFieldP field f
    = TypeError
        (     'ShowType f
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

type family HasTypeP a f :: Bool where
  HasTypeP t (S1 meta (Rec0 t))
    = 'True
  HasTypeP t (l :*: r)
    = HasTypeP t l || HasTypeP t r
  HasTypeP t (l :+: r)
    = HasTypeP t l && HasTypeP t r
  HasTypeP t (S1 _ _)
    = 'False
  HasTypeP t (C1 m f)
    = HasTypeP t f
  HasTypeP t (D1 m f)
    = HasTypeP t f
  HasTypeP t (Rec0 _)
    = 'False
  HasTypeP t U1
    = 'False
  HasTypeP t V1
    = 'False
  HasTypeP t f
    = TypeError
        (     'ShowType f
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

type family (a :: Bool) || (b :: Bool) :: Bool where
  'True || _ = 'True
  _     || b = b

type family (a :: Bool) && (b :: Bool) :: Bool where
  'True && 'True = 'True
  _     && _     = 'False

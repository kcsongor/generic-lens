{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generics.Internal.Families.Find
  ( FindField
  , FindType
  ) where

import Data.Kind    (Type)
import GHC.Generics
import GHC.TypeLits

-- | Look up a record field by name in the generic representation, and return
--   its corresponding type, if exists.
type family FindField (field :: Symbol) f :: Maybe Type where
  FindField field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'Just t
  FindField field (l :*: r)
    = FindField field l <|> FindField field r
  FindField field (l :+: r)
    = FindField field l <&> FindField field r
  FindField field (S1 _ _)
    = 'Nothing
  FindField field (C1 m f)
    = FindField field f
  FindField field (D1 m f)
    = FindField field f
  FindField field (Rec0 _)
    = 'Nothing
  FindField field U1
    = 'Nothing
  FindField field V1
    = 'Nothing
  FindField field f
    = TypeError
        (     'ShowType f
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

type family FindType a f :: Maybe Type where
  FindType t (S1 meta (Rec0 t))
    = 'Just t
  FindType t (l :*: r)
    = FindType t l <|> FindType t r
  FindType t (l :+: r)
    = FindType t l <&> FindType t r
  FindType t (S1 _ _)
    = 'Nothing
  FindType t (C1 m f)
    = FindType t f
  FindType t (D1 m f)
    = FindType t f
  FindType t (Rec0 _)
    = 'Nothing
  FindType t U1
    = 'Nothing
  FindType t V1
    = 'Nothing
  FindType t f
    = TypeError
        (     'ShowType f
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

-- | Type-level alternative monomorphised to @Maybe@.
type family (a :: Maybe k) <|> (b :: Maybe k) :: Maybe k where
  'Just a <|> _ = 'Just a
  _       <|> b = b

type family (a :: Maybe k) <&> (b :: Maybe k) :: Maybe k where
  'Just a <&> 'Just a = 'Just a
  _       <&> _       = 'Nothing

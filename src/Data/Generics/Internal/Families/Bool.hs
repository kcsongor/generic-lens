{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Data.Generics.Internal.Families.Bool
  ( type (||)
  , type (&&)
  ) where

type family (a :: Bool) || (b :: Bool) :: Bool where
  'True || _ = 'True
  _     || b = b

type family (a :: Bool) && (b :: Bool) :: Bool where
  'True && 'True = 'True
  _     && _     = 'False

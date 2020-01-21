{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.Families
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Families
  ( module Families
  , ShowSymbols
  ) where

import Data.Generics.Internal.Families.Collect   as Families
import Data.Generics.Internal.Families.Has       as Families
import Data.Generics.Internal.Families.Changing  as Families

import GHC.TypeLits (ErrorMessage (..), Symbol)

type family ShowSymbols (ctors :: [Symbol]) :: ErrorMessage where
  ShowSymbols '[]
    = 'Text ""
  ShowSymbols (c ': cs)
    = 'Text "• " ':<>: 'Text c ':$$: ShowSymbols cs

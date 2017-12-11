{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans       #-}


--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Labels
-- Copyright   : (C) 2017 Csongor Kiss
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides an (orphan) IsLabel instance for field lenses.
-- Use at your own risk.
--------------------------------------------------------------------------------

module Data.Generics.Labels () where

import Data.Generics.Product
import GHC.OverloadedLabels


instance (HasField field s t a b, Functor f, sft ~ (s -> f t)) =>
  IsLabel field ((a -> f b) -> sft) where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = field @field
#else
  fromLabel _ = field @field
#endif

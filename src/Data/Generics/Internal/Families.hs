{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.Families
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Families
  ( module Families
  , ShowConstuctors
  ) where

import Data.Generics.Internal.Families.Collect   as Families
import Data.Generics.Internal.Families.Has       as Families
import Data.Generics.Internal.Families.Changing  as Families

import GHC.TypeLits (ErrorMessage (..), Symbol)

type family ShowConstuctors (ctors :: [Symbol]) :: ErrorMessage where
  ShowConstuctors '[]
    = 'Text ""
  ShowConstuctors (c ': cs)
    = 'Text "* " ':<>: 'Text c ':$$: ShowConstuctors cs

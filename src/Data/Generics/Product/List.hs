{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.List
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a product type and a flat HList.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.List
  ( IsList (..)
  , IsRecord
  , IsRecord'
  ) where

import Data.Generics.Product.Internal.List
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Data.Generics.Internal.Profunctor.Iso

class IsList
  (m :: Type)
  (f :: Type)
  (g :: Type)
  (as :: [(m, Type)])
  (bs :: [(m, Type)]) | m f -> as, m g -> bs where
  list  :: Iso f g (List as) (List bs)

instance
  ( Generic f
  , Generic g
  , GIsList m (Rep f) (Rep g) as bs
  ) => IsList m f g as bs where
  list = repIso . glist @m
  {-# INLINE list #-}

class IsList Symbol f g as bs => IsRecord f g (as :: [(Symbol, Type)]) (bs :: [(Symbol, Type)]) | f -> as, g -> bs
instance IsList Symbol f g as bs => IsRecord f g as bs

class IsRecord f f as as => IsRecord' f (as :: [(Symbol, Type)]) | f -> as
instance IsRecord f f as as => IsRecord' f as

-- example (TODO: move elsewhere)
--class PrintRecord (rl :: [(Symbol, Type)]) where
--  printRecord' :: List rl -> String
--
--instance PrintRecord '[] where
--  printRecord' _ = ""
--
--instance (KnownSymbol field, Show a, PrintRecord xs) => PrintRecord ('(field, a) ': xs) where
--  printRecord' (x :> xs) = show x ++ ", " ++ printRecord' xs
--
--printRecord :: (IsRecord' rec rl, PrintRecord rl) => rec -> String
--printRecord rec = printRecord' (rec ^. list)
--
--data MyRecord = MyRecord
--  { field1 :: Int
--  , field2 :: String
--  , field3 :: Bool
--  } deriving Generic
--
-- >>> printRecord (MyRecord 10 "hello" False)
-- "10, \"hello\", False, "

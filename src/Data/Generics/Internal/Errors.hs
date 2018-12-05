{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
---- |
---- Module      :  Data.GenericLens.Errors
---- Copyright   :  (C) 2018 Csongor Kiss
---- License     :  BSD3
---- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
---- Stability   :  experimental
---- Portability :  non-portable
----
---- Provide more legible type errors
-------------------------------------------------------------------------------

module Data.Generics.Internal.Errors
 ( NoGeneric
 , Defined
 ) where

import GHC.Generics
import GHC.TypeLits
import Data.Kind

type family NoGeneric (a :: Type) (ctxt :: ErrorMessage) :: Constraint where
  NoGeneric a ctxt = TypeError
                    ('Text "No instance for " ':<>: 'ShowType (Generic a)
                     ':$$: ctxt
                    )

type family Defined (break :: Type -> Type) (err :: Constraint) (a :: k) :: k where
  Defined Void1 _ _ = Any
  Defined _ _     k = k

data Void1 a
type family Any :: k

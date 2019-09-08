{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
---- |
---- Module      :  Data.Generics.Internal.Errors
---- Copyright   :  (C) 2019 Csongor Kiss
---- License     :  BSD3
---- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
---- Stability   :  experimental
---- Portability :  non-portable
----
---- Provide more legible type errors as described in
---- (https://kcsongor.github.io/report-stuck-families/)[this blog post].
-------------------------------------------------------------------------------

module Data.Generics.Internal.Errors
 ( NoGeneric
 , Defined
 , Defined_list

 , QuoteType
 , PrettyError
 ) where

import GHC.Generics
import GHC.TypeLits
import Data.Kind

type family NoGeneric (a :: Type) (ctxt :: [ErrorMessage]) :: Constraint where
  NoGeneric a ctxt = PrettyError ('Text "No instance for " ':<>: QuoteType (Generic a)
                                   ': ctxt)
type family PrettyError (ctxt :: [ErrorMessage]) :: k where
  PrettyError '[] = TypeError ('Text "")
  PrettyError (c ': cs) = TypeError ('Text "| " ':<>: c ':$$: PrettyLines cs)

type family PrettyLines (ctxt :: [ErrorMessage]) :: ErrorMessage where
  PrettyLines '[] = 'Text ""
  PrettyLines (c ': cs) = 'Text "|   " ':<>: c ':$$: PrettyLines cs

type family Defined (break :: Type -> Type) (err :: Constraint) (a :: k) :: k where
  Defined Void1 _ _ = Any
  Defined _ _     k = k

type family Defined_list (break :: [*]) (err :: Constraint) (a :: k) :: k where
  Defined_list '[Void] _ _ = Any
  Defined_list _ _        k = k

data Void1 a
data Void
type family Any :: k

type family QuoteType (typ :: k) :: ErrorMessage where
  QuoteType typ = 'Text "‘" ':<>: 'ShowType typ ':<>: 'Text "’"

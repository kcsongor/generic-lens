{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Product.Param
-- Copyright   : (C) 2018 Csongor Kiss
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- Derive traversal over type parameters
--
--------------------------------------------------------------------------------

module Data.Generics.Product.Param where

import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (Nat)
import GHC.Generics
import Data.Generics.Internal.Families.Changing
import Data.Generics.Product.Internal.Param

class HasParam (p :: Nat) s t a b | p t a b -> s, p s a b -> t where
  param :: Applicative g => (a -> g b) -> s -> g t

instance
  ( p ~ NatToPeano n
  , Generic s
  , Generic s'
  , Generic t'
  , '(a', b') ~ '(P p a 'PTag, P p b 'PTag)
  , '(s', t') ~ '(Proxied s, Proxied t)
  , s ~ Infer t b' a
  , t ~ Infer s a' b
  , GHasParam p (Rep s') (Rep t') a' b'
  ) => HasParam n s t a b where

  -- TODO: I think unsafeCoerce is not necessary
  param f s
    = (unsafeCoerce @t' @t . to) <$> gparam @p (lift f) (from (unsafeCoerce @s @s' s))
    where lift :: (a -> g b) -> P p a 'PTag -> g (P p b 'PTag)
          lift = unsafeCoerce


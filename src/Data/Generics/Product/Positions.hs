{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Positions
-- Copyright   :  (C) 2019 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive positional product type getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Positions
  ( -- *Lenses

    -- $setup
    HasPosition (..)
  , HasPosition' (..)
  , HasPosition_ (..)
  , HasPosition0 (..)

  , getPosition
  , setPosition
  ) where

-- import Data.Generics.Internal.VL.Lens as VL
import Data.Generics.Internal.Void
import Data.Generics.Internal.Families
import Data.Generics.Product.Internal.Positions
import Data.Generics.Product.Internal.GLens
import Data.Generics.Internal.Errors

import Data.Kind      (Constraint, Type)
import Data.Type.Bool (type (&&))
import GHC.Generics
import GHC.TypeLits   (type (<=?),  Nat, TypeError, ErrorMessage(..))
import Data.Generics.Internal.Profunctor.Lens as P
import Data.Coerce

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.VL.Lens
-- >>> :m +Data.Function
-- >>> :{
-- data Human = Human
--   { name    :: String
--   , age     :: Int
--   , address :: String
--   }
--   deriving (Generic, Show)
-- human :: Human
-- human = Human "Tunyasz" 50 "London"
-- :}

-- |Records that have a field at a given position.
class HasPosition (i :: Nat) s t a b | s i -> a, t i -> b, s i b -> t, t i a -> s where
  -- |A lens that focuses on a field at a given position. Compatible with the
  --  lens package's 'Control.Lens.Lens' type.
  --
  --  >>> human ^. position @1
  --  "Tunyasz"
  --  >>> human & position @3 .~ "Berlin"
  --  Human {name = "Tunyasz", age = 50, address = "Berlin"}
  --
  --  === /Type errors/
  --
  --  >>> human & position @4 .~ "Berlin"
  --  ...
  --  ... The type Human does not contain a field at position 4
  --  ...
  position :: Lens s t a b

class HasPosition_ (i :: Nat) s t a b where
  position_ :: Lens s t a b

-- |Records that have a field at a given position.
--
-- The difference between 'HasPosition' and 'HasPosition_' is similar to the
-- one between 'Data.Generics.Product.Fields.HasField' and
-- 'Data.Generics.Product.Fields.HasField_'.
-- See 'Data.Generics.Product.Fields.HasField_'.
class HasPosition' (i :: Nat) s a | s i -> a where
  position' :: Lens s s a a

-- |Records that have a field at a given position.
--
-- This class gives the minimal constraints needed to define this lens.
-- For common uses, see 'HasPosition'.
class HasPosition0 (i :: Nat) s t a b where
  position0 :: Lens s t a b

-- |
-- >>> getPosition @2 human
-- 50
getPosition :: forall i s a. HasPosition' i s a => s -> a
getPosition s = view (position' @i) s

-- |
-- >>> setPosition @2 60 human
-- Human {name = "Tunyasz", age = 60, address = "London"}
setPosition :: forall i s a. HasPosition' i s a => a -> s -> s
setPosition = set (position' @i)

instance
  ( Generic s
  , ErrorUnless i s (0 <? i && i <=? Size (Rep s))
  , cs ~ CRep s
  , Coercible (Rep s) cs
  , GLens' (HasTotalPositionPSym i) cs a
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic lens focusing on the field at"
                  , 'Text "position " ':<>: QuoteType i ':<>: 'Text " of type " ':<>: QuoteType a
                    ':<>: 'Text " in " ':<>: QuoteType s
                  ])
    (() :: Constraint)
  ) => HasPosition' i s a where
  position' f = ravel (repLens . coerced @cs @cs . glens @(HasTotalPositionPSym i)) f
  {-# INLINE position' #-}

-- this is to 'hide' the equality constraints which interfere with inlining
-- pre 8.4.3
class (~~) (a :: k) (b :: k) | a -> b, b -> a
instance (a ~ b) => (~~) a b

instance  -- see Note [Changing type parameters]
  ( ErrorUnless i s (0 <? i && i <=? Size (Rep s))
  , HasTotalPositionP i (CRep s) ~~ 'Just a
  , HasTotalPositionP i (CRep t) ~~ 'Just b
  , HasTotalPositionP i (CRep (Indexed s)) ~~ 'Just a'
  , HasTotalPositionP i (CRep (Indexed t)) ~~ 'Just b'
  , t ~~ Infer s a' b
  , s ~~ Infer t b' a
  , Coercible (CRep s) (Rep s)
  , Coercible (CRep t) (Rep t)
  , HasPosition0 i s t a b
  ) => HasPosition i s t a b where

  position = position0 @i
  {-# INLINE position #-}

-- We wouldn't need the universal 'x' here if we could express above that
-- forall x. Coercible (cs x) (Rep s x), but this requires quantified
-- constraints
coerced :: forall s t is s' t' x a b. (Coercible t t', Coercible s s')
        => P.ALens a b is (s x) (t x) -> P.ALens a b is (s' x) (t' x)
coerced = coerce
{-# INLINE coerced #-}

-- | See Note [Uncluttering type signatures]
#if __GLASGOW_HASKELL__ < 804
-- >>> :t position
-- position
--   :: (HasPosition i s t a b, Functor f) => (a -> f b) -> s -> f t
#else
-- >>> :t position
-- position
--   :: (Functor f, HasPosition i s t a b) => (a -> f b) -> s -> f t
#endif
instance {-# OVERLAPPING #-} HasPosition f (Void1 a) (Void1 b) a b where
  position = undefined

instance
  ( ErrorUnless i s (0 <? i && i <=? Size (Rep s))
  , UnifyHead s t
  , UnifyHead t s
  , Coercible (CRep s) (Rep s)
  , Coercible (CRep t) (Rep t)
  , HasPosition0 i s t a b
  ) => HasPosition_ i s t a b where

  position_ = position0 @i
  {-# INLINE position_ #-}

instance {-# OVERLAPPING #-} HasPosition_ f (Void1 a) (Void1 b) a b where
  position_ = undefined

instance
  ( Generic s
  , Generic t
  , GLens (HasTotalPositionPSym i) (CRep s) (CRep t) a b
  , Coercible (CRep s) (Rep s)
  , Coercible (CRep t) (Rep t)
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic lens focusing on the field at"
                  , 'Text "position " ':<>: QuoteType i ':<>: 'Text " of type " ':<>: QuoteType a
                    ':<>: 'Text " in " ':<>: QuoteType s
                  ])
    (() :: Constraint)
  ) => HasPosition0 i s t a b where
  position0 = ravel (repLens . coerced @(CRep s) @(CRep t) . glens @(HasTotalPositionPSym i))
  {-# INLINE position0 #-}

type family ErrorUnless (i :: Nat) (s :: Type) (hasP :: Bool) :: Constraint where
  ErrorUnless i s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field at position "
        ':<>: 'ShowType i
        )

  ErrorUnless _ _ 'True
    = ()

data HasTotalPositionPSym  :: Nat -> (TyFun (Type -> Type) (Maybe Type))
type instance Eval (HasTotalPositionPSym t) tt = HasTotalPositionP t tt

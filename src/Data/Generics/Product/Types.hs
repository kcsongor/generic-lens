{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Types
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Types where

  --( -- *Traversals
  --  --
  --  --  $example
  --  HasTypes (..)
  --, HasTypesDeep (..)
  --, Node (..)
  --, Primitive (..)
  --) where

import Data.Generics.Product.Internal.Types
import Data.Generics.Internal.GenericDeep
import Data.Type.Bool
import Data.Kind

import GHC.Generics
import Data.Generics.Internal.VL.Traversal
import Data.Generics.Internal.VL.Iso

class HasTypes a s where
  types :: Traversal' s a

instance
  ( Generic s
  , GHasTypes (Rep s) '[a]
  ) => HasTypes a s where
  types = confusing (\f -> (repIso . gtypes) (f :> HNil))
  {-# INLINE types #-}

class HasTypesDeep a s where
  typesDeep :: Traversal' s a

  default typesDeep :: Traversal' s a
  typesDeep _ = pure

instance
  ( GHasTypesDeep is a (Rep s)
  , is ~ FindInteresting (Listify (DeepN s)) a
  , GenericDeep s
  ) => HasTypesDeep a s where
  typesDeep f s = to <$> gtypesDeep @is f (from s)
  {-# INLINE[2] typesDeep #-}


instance {-# OVERLAPPING #-} HasTypesDeep a Bool
instance {-# OVERLAPPING #-} HasTypesDeep a Char
instance {-# OVERLAPPING #-} HasTypesDeep a Double
instance {-# OVERLAPPING #-} HasTypesDeep a Float
instance {-# OVERLAPPING #-} HasTypesDeep a Int
instance {-# OVERLAPPING #-} HasTypesDeep a Integer
instance {-# OVERLAPPING #-} HasTypesDeep a Ordering

--------------------------------------------------------------------------------

class GHasTypesDeep (is :: [Type]) a s where
  gtypesDeep :: Traversal' (s x) a

instance GHasTypesDeep is a U1 where
  gtypesDeep _ = pure
  {-# INLINE[0] gtypesDeep #-}

instance {-# OVERLAPPING #-} GHasTypesDeep is a (Rec0 a) where
  gtypesDeep f (K1 x) = K1 <$> f x
  {-# INLINE[0] gtypesDeep #-}

instance GHasTypesDeep is a f => GHasTypesDeep is a (M1 met m f) where
  gtypesDeep f (M1 s) = M1 <$> gtypesDeep @is f s
  {-# INLINE[0] gtypesDeep #-}

instance {-# OVERLAPPABLE #-}
  ( HasTypesDeep' is (Elem b is) a b
  ) => GHasTypesDeep (is :: [Type]) a (Rec0 b) where
  gtypesDeep f (K1 x) = K1 <$> typesDeep' @is @(Elem b is) f x
  {-# INLINE[0] gtypesDeep #-}

class HasTypesDeep' (is :: [Type]) b a s where
  typesDeep' :: Traversal' s a

instance HasTypesDeep' is 'False a s where
  typesDeep' _ = pure
  {-# INLINE[0] typesDeep' #-}

instance (Generic s, GHasTypesDeep is a (Rep s))
  => HasTypesDeep' is 'True a s where
  typesDeep' f s = to <$> gtypesDeep @is f (from s)
  {-# INLINE[0] typesDeep' #-}

instance
  ( GHasTypesDeep is a l
  , GHasTypesDeep is a r
  ) => GHasTypesDeep is a (l :*: r) where
  gtypesDeep f (l :*: r) = (:*:) <$> gtypesDeep @is f l <*> gtypesDeep @is f r
  {-# INLINE[0] gtypesDeep #-}

instance
  ( GHasTypesDeep is a l
  , GHasTypesDeep is a r
  ) => GHasTypesDeep is a (l :+: r) where
  gtypesDeep f (L1 l) = L1 <$> gtypesDeep @is f l
  gtypesDeep f (R1 r) = R1 <$> gtypesDeep @is f r
  {-# INLINE[0] gtypesDeep #-}

---------

type DeepN t = Node t (Deep t)

--------------------------------------------------------------------------------
type Set = [Type]

type EmptySet = '[]

type family Mark (k :: Type) (m :: Set) :: Set where
  Mark k '[] = '[ k ]
  Mark k (k ': ms) = k ': ms
  Mark k (m ': ms) = m ': Mark k ms

type family MarkAll (ts :: [Type]) (m :: Set) :: Set where
  MarkAll '[] m = m
  MarkAll (t ': ts) m = MarkAll ts (Mark t m)

type family IsInteresting (t :: Type) (m :: Set) :: Bool where
  IsInteresting t ( t ': _ ': _ ': _ ': _ ) = 'True
  IsInteresting t ( _ ': t ': _ ': _ ': _ ) = 'True
  IsInteresting t ( _ ': _ ': t ': _ ': _ ) = 'True
  IsInteresting t ( _ ': _ ': _ ': t ': _ ) = 'True
  IsInteresting t ( _ ': _ ': _ ': _ ': m ) = IsInteresting t m
  IsInteresting _ _ = 'False

--------------------------------------------------------------------------------

type family FindInteresting' (haystack :: [Type -> Type]) (needle :: Type) (path :: [Type]) (m :: Set) :: Set where
  FindInteresting' '[] a p map
    = map
  FindInteresting' (Node a tree ': xs) a p map
    --                          v optimisation
    = FindInteresting' (tree ': xs) a '[a] (MarkAll p map)
  FindInteresting' (Node typ tree ': xs) a p map
    = FindInteresting' (tree ': xs) a (typ ': p) map
--  FindInteresting' ((l :*: r) ': xs) a p map
--    = FindInteresting' (l ': r ': xs) a p map
     -- = FindInteresting' l a p map
--  FindInteresting' ((l :+: r) ': xs) a p map
--    = FindInteresting' (l ': r ': xs) a p map
    -- = FindInteresting' l a p map
  FindInteresting' (Primitive a ': xs) a p map
    = FindInteresting' xs a p (MarkAll p map)
  FindInteresting' (Primitive typ ': xs) a p map
    = FindInteresting' xs a p map
  FindInteresting' (Rec0 a ': xs) a p map
    = FindInteresting' xs a p (MarkAll p map)
  FindInteresting' (Rec0 typ ': xs) a p map
    = FindInteresting' xs a p (If (IsInteresting typ map) (MarkAll p map) map)
  FindInteresting' (U1 ': xs) a p map
    = FindInteresting' xs a p map

type FindInteresting hay need
  = Fix hay need (FindInteresting' hay need '[] EmptySet) EmptySet

type family ((as :: [k]) ++ (bs :: [k])) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': as ++ bs

type family Any :: k

type family DList :: [xs] -> [xs]
data List a = Comp (List a) (List a) | Leaf a

type family ToList (xs :: List [a]) :: [a] where
  ToList ('Leaf x) = x
  ToList ('Comp ('Leaf x) y) = x ++ ToList y
  ToList ('Comp ('Comp x y) z)
    = ToList ('Comp x ('Comp y z))

type family Listify2 a :: List [Type] where
  Listify2 (l :+: r) = 'Comp (Listify2 l) (Listify2 r)
  Listify2 (l :*: r) = 'Comp (Listify2 l) (Listify2 r)
  --Listify2 (Node a tree) = Comp (DList (Node1 a))  (Comp (Listify2 tree) '[StopNode1])
  Listify2 (Node a tree) = 'Comp ('Comp ('Leaf '[Node2 a]) (Listify2 tree)) ('Leaf '[StopNode2])
  Listify2 (Primitive a) = 'Leaf '[a]
  Listify2 U1 = 'Leaf '[]
  Listify2 (Rec0 a) = 'Leaf '[a]
--  Listify2 x = '[x]

data Node2 a
data StopNode2

type family Listify a where
  Listify (l :+: r) = Any "++" (Listify l) (Listify r)
  Listify (l :*: r) = Any "++" (Listify l) (Listify r)
  Listify (Node a tree) = Node1 a ': Listify tree ++ '[StopNode1]
  Listify x = '[x]

data Node1 a x
data StopNode1 a

type family Fix hay need m n where
  Fix _ _ m m = m
  Fix haystack needle m _
    = Fix haystack needle (FindInteresting' haystack needle '[] m) m

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module DList where

type L f = (Unit f, Concat f)

class Concat f where
  conc :: f a -> f a -> f a

class Unit f where
  unit :: a -> f a
  nil :: f a

instance Concat [] where
  conc = (++)

instance Unit [] where
  unit a = [a]
  nil = []


newtype StringWrapper a = StringWrapper String deriving Show

instance Concat StringWrapper where
  conc (StringWrapper s1) (StringWrapper s2) =
    StringWrapper ("( " ++  s1 ++ " ++ " ++ s2 ++ " )")

instance Unit StringWrapper where
  unit a = StringWrapper "[val]"
  nil = StringWrapper "[]"

cons x fa = unit x `conc` fa

u :: L f => a -> f a
u = unit


l1 :: forall f . (Unit f, Concat f) => f Int
l1 = ((u 1) `conc` (u 2)) `conc` ((u 3) `conc` (u 4))

-- Observe the normalisation using l1' @StringWrapper
l1' :: forall f . L f => f Int
l1' = ravel l1


-- Goal: Define a data type Norm such that Norm f a ~ f a but Norm
-- right associates the cons

newtype MapK f a = MapK (f a -> f a)

liftMapK :: Concat f => f a -> MapK f a
--liftMapK fa = (MapK (`conc` fa)) -- Right Associative
liftMapK fa = (MapK (fa `conc`)) -- Left Associative

lowerMapK :: Unit f => MapK f a -> f a
lowerMapK (MapK fa) = fa nil

(<<++>) :: f a -> MapK f a -> f a
(<<++>) fa (MapK k) = k fa

instance  Concat (MapK f) where
--  conc fa fb  = MapK (\g -> g <<++> fa <<++> fb)
    conc (MapK fa) (MapK fb) = MapK (fa . fb)
--  unit a = MapK (\fa -> unit a

instance L f => (Unit (MapK f)) where
  unit a = liftMapK (unit a)
  nil = liftMapK nil

type List a = forall f . L f => f a

ravel :: List a -> List a
ravel fa = lowerMapK fa

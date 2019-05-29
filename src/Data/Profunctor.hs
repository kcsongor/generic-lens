{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.Profunctor (Profunctor(..), Strong(..), Choice(..)) where
import Data.Profunctor.Unsafe
import Data.Tuple
import Data.Tagged

class Profunctor p => Strong p where
  -- | Laws:
  --
  -- @
  -- 'first'' ≡ 'dimap' 'swap' 'swap' '.' 'second''
  -- 'lmap' 'fst' ≡ 'rmap' 'fst' '.' 'first''
  -- 'lmap' ('second'' f) '.' 'first'' ≡ 'rmap' ('second'' f) '.' 'first''
  -- 'first'' '.' 'first'' ≡ 'dimap' assoc unassoc '.' 'first'' where
  --   assoc ((a,b),c) = (a,(b,c))
  --   unassoc (a,(b,c)) = ((a,b),c)
  -- @
  first' :: p a b  -> p (a, c) (b, c)
  first' = dimap swap swap . second'

  -- | Laws:
  --
  -- @
  -- 'second'' ≡ 'dimap' 'swap' 'swap' '.' 'first''
  -- 'lmap' 'snd' ≡ 'rmap' 'snd' '.' 'second''
  -- 'lmap' ('first'' f) '.' 'second'' ≡ 'rmap' ('first'' f) '.' 'second''
  -- 'second'' '.' 'second'' ≡ 'dimap' unassoc assoc '.' 'second'' where
  --   assoc ((a,b),c) = (a,(b,c))
  --   unassoc (a,(b,c)) = ((a,b),c)
  -- @
  second' :: p a b -> p (c, a) (c, b)
  second' = dimap swap swap . first'

  {-# MINIMAL first' | second' #-}


------------------------------------------------------------------------------
-- Choice
------------------------------------------------------------------------------

-- | The generalization of 'Costar' of 'Functor' that is strong with respect
-- to 'Either'.
--
-- Note: This is also a notion of strength, except with regards to another monoidal
-- structure that we can choose to equip Hask with: the cocartesian coproduct.
class Profunctor p => Choice p where
  -- | Laws:
  --
  -- @
  -- 'left'' ≡ 'dimap' swapE swapE '.' 'right'' where
  --   swapE :: 'Either' a b -> 'Either' b a
  --   swapE = 'either' 'Right' 'Left'
  -- 'rmap' 'Left' ≡ 'lmap' 'Left' '.' 'left''
  -- 'lmap' ('right' f) '.' 'left'' ≡ 'rmap' ('right' f) '.' 'left''
  -- 'left'' '.' 'left'' ≡ 'dimap' assocE unassocE '.' 'left'' where
  --   assocE :: 'Either' ('Either' a b) c -> 'Either' a ('Either' b c)
  --   assocE ('Left' ('Left' a)) = 'Left' a
  --   assocE ('Left' ('Right' b)) = 'Right' ('Left' b)
  --   assocE ('Right' c) = 'Right' ('Right' c)
  --   unassocE :: 'Either' a ('Either' b c) -> 'Either' ('Either' a b) c
  --   unassocE ('Left' a) = 'Left' ('Left' a)
  --   unassocE ('Right' ('Left' b) = 'Left' ('Right' b)
  --   unassocE ('Right' ('Right' c)) = 'Right' c)
  -- @
  left'  :: p a b -> p (Either a c) (Either b c)
  left' =  dimap (either Right Left) (either Right Left) . right'

  -- | Laws:
  --
  -- @
  -- 'right'' ≡ 'dimap' swapE swapE '.' 'left'' where
  --   swapE :: 'Either' a b -> 'Either' b a
  --   swapE = 'either' 'Right' 'Left'
  -- 'rmap' 'Right' ≡ 'lmap' 'Right' '.' 'right''
  -- 'lmap' ('left' f) '.' 'right'' ≡ 'rmap' ('left' f) '.' 'right''
  -- 'right'' '.' 'right'' ≡ 'dimap' unassocE assocE '.' 'right'' where
  --   assocE :: 'Either' ('Either' a b) c -> 'Either' a ('Either' b c)
  --   assocE ('Left' ('Left' a)) = 'Left' a
  --   assocE ('Left' ('Right' b)) = 'Right' ('Left' b)
  --   assocE ('Right' c) = 'Right' ('Right' c)
  --   unassocE :: 'Either' a ('Either' b c) -> 'Either' ('Either' a b) c
  --   unassocE ('Left' a) = 'Left' ('Left' a)
  --   unassocE ('Right' ('Left' b) = 'Left' ('Right' b)
  --   unassocE ('Right' ('Right' c)) = 'Right' c)
  -- @
  right' :: p a b -> p (Either c a) (Either c b)
  right' =  dimap (either Right Left) (either Right Left) . left'

  {-# MINIMAL left' | right' #-}

instance Choice (->) where
  left' ab (Left a) = Left (ab a)
  left' _ (Right c) = Right c
  {-# INLINE left' #-}
  right' = fmap
  {-# INLINE right' #-}

instance Choice Tagged where
  left' (Tagged b) = Tagged (Left b)
  {-# INLINE left' #-}
  right' (Tagged b) = Tagged (Right b)
  {-# INLINE right' #-}

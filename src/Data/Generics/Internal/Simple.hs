{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Internal.Simple
-- Copyright   : (C) 2018 Csongor Kiss
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- Simpler Generics representation.
--
--------------------------------------------------------------------------------

module Data.Generics.Internal.Simple where

import Data.Generics.Internal.VL.Lens
import Data.Kind
import GHC.TypeLits
import Unsafe.Coerce
import qualified GHC.Generics as G

data U1                  = U1
data V1
data f :*: g             = f :*: g
data f :+: g             = L1 f | R1 g
newtype M1 (m :: Meta) a = M1 { unM1 :: a }
newtype Rec0 a           = K1 { unK1 :: a }

data Meta  =  MetaData Symbol
           |  MetaCons Symbol
           |  MetaSel (Maybe Symbol)

class G.Generic a => Generic a where
  type family Rep a :: Type
  type instance Rep a = OurRep (G.Rep a ())
  from :: a   -> Rep a
  to :: Rep a -> a

instance G.Generic a => Generic a where
  from = unsafeCoerce (G.from @a @())
  {-# INLINE[0] from #-}
  to   = unsafeCoerce (G.to @a @())
  {-# INLINE[0] to #-}

repIso :: (Generic s, Generic t) => Lens s t (Rep s) (Rep t)
repIso f = fmap to . f . from
{-# INLINE[0] repIso #-}

kIso :: Lens (Rec0 a) (Rec0 b) a b
kIso f s = K1 <$> f (unK1 s)
{-# INLINE[0] kIso #-}

mIso :: Lens (M1 m s) (M1 m t) s t
mIso f s = M1 <$> f (unM1 s)
{-# INLINE[0] mIso #-}

type family OurRep a  where
  OurRep (G.M1 _ ('G.MetaData dataname _ _ _) f x)
    = M1 ('MetaData dataname) (OurRep (f x))
  OurRep (G.M1 _ ('G.MetaSel sel _ _ _) f x)
    = M1 ('MetaSel sel) (OurRep (f x))
  OurRep (G.M1 _ ('G.MetaCons conname _ _) f x)
    = M1 ('MetaCons conname) (OurRep (f x))
  OurRep ((l G.:+: r) x)
    = OurRep (l x) :+: OurRep (r x)
  OurRep ((l G.:*: r) x)
    = OurRep (l x) :*: OurRep (r x)
  OurRep (G.K1 _ t _)
    = Rec0 t
  OurRep (G.V1 _)
    = V1
  OurRep (G.U1 _)
    = U1

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

#ifndef LENS
#define LENS 1
#endif

#ifndef GENERIC
#define GENERIC 1
#endif

#ifndef MULTIPLE
#define MULTIPLE 3
#endif

#ifndef xfield
#define xfield field'
#endif

#ifndef xctor
#define xctor _Ctor'
#endif

import Control.DeepSeq
import Criterion.Main
import Criterion.Types
import "generic-optics" Data.Generics.Product
import "generic-optics" Data.Generics.Sum
import GHC.Generics
import Optics.Core
import Optics.TH

main :: IO ()
main = pure ()


----------------------------------------

#if LENS == 1

data T0 = T0
  { t0_f0 :: Int
  , t0_f1 :: Int
  , t0_f2 :: Int
  , t0_f3 :: Int
  , t0_f4 :: Int
  , t0_f5 :: Int
  , t0_f6 :: Int
  , t0_f7 :: Int
  , t0_f8 :: Int
  , t0_f9 :: Int
  }
#if MULTIPLE >= 1
  | T01 { t0_f0 :: Int
        , t0_f1 :: Int
        , t0_f2 :: Int
        , t0_f3 :: Int
        , t0_f4 :: Int
        , t0_f5 :: Int
        , t0_f6 :: Int
        , t0_f7 :: Int
        , t0_f8 :: Int
        , t0_f9 :: Int
        }
#endif
#if MULTIPLE >= 2
  | T02 { t0_f0 :: Int
        , t0_f1 :: Int
        , t0_f2 :: Int
        , t0_f3 :: Int
        , t0_f4 :: Int
        , t0_f5 :: Int
        , t0_f6 :: Int
        , t0_f7 :: Int
        , t0_f8 :: Int
        , t0_f9 :: Int
        }
#endif
#if MULTIPLE >= 3
  | T03 { t0_f0 :: Int
        , t0_f1 :: Int
        , t0_f2 :: Int
        , t0_f3 :: Int
        , t0_f4 :: Int
        , t0_f5 :: Int
        , t0_f6 :: Int
        , t0_f7 :: Int
        , t0_f8 :: Int
        , t0_f9 :: Int
        }
#endif
#if GENERIC == 1
  deriving Generic
#endif

data T1 = T1
  { t1_f0 :: Int
  , t1_f1 :: Int
  , t1_f2 :: Int
  , t1_f3 :: Int
  , t1_f4 :: Int
  , t1_f5 :: Int
  , t1_f6 :: Int
  , t1_f7 :: Int
  , t1_f8 :: Int
  , t1_f9 :: Int
  }
#if MULTIPLE >= 1
  | T11 { t1_f0 :: Int
        , t1_f1 :: Int
        , t1_f2 :: Int
        , t1_f3 :: Int
        , t1_f4 :: Int
        , t1_f5 :: Int
        , t1_f6 :: Int
        , t1_f7 :: Int
        , t1_f8 :: Int
        , t1_f9 :: Int
        }
#endif
#if MULTIPLE >= 2
  | T12 { t1_f0 :: Int
        , t1_f1 :: Int
        , t1_f2 :: Int
        , t1_f3 :: Int
        , t1_f4 :: Int
        , t1_f5 :: Int
        , t1_f6 :: Int
        , t1_f7 :: Int
        , t1_f8 :: Int
        , t1_f9 :: Int
        }
#endif
#if MULTIPLE >= 3
  | T13 { t1_f0 :: Int
        , t1_f1 :: Int
        , t1_f2 :: Int
        , t1_f3 :: Int
        , t1_f4 :: Int
        , t1_f5 :: Int
        , t1_f6 :: Int
        , t1_f7 :: Int
        , t1_f8 :: Int
        , t1_f9 :: Int
        }
#endif
#if GENERIC == 1
  deriving Generic
#endif

data T2 = T2
  { t2_f0 :: Int
  , t2_f1 :: Int
  , t2_f2 :: Int
  , t2_f3 :: Int
  , t2_f4 :: Int
  , t2_f5 :: Int
  , t2_f6 :: Int
  , t2_f7 :: Int
  , t2_f8 :: Int
   , t2_f9 :: Int
  }
#if MULTIPLE >= 1
  | T21 { t2_f0 :: Int
        , t2_f1 :: Int
        , t2_f2 :: Int
        , t2_f3 :: Int
        , t2_f4 :: Int
        , t2_f5 :: Int
        , t2_f6 :: Int
        , t2_f7 :: Int
        , t2_f8 :: Int
        , t2_f9 :: Int
        }
#endif
#if MULTIPLE >= 2
  | T22 { t2_f0 :: Int
        , t2_f1 :: Int
        , t2_f2 :: Int
        , t2_f3 :: Int
        , t2_f4 :: Int
        , t2_f5 :: Int
        , t2_f6 :: Int
        , t2_f7 :: Int
        , t2_f8 :: Int
        , t2_f9 :: Int
        }
#endif
#if MULTIPLE >= 3
  | T23 { t2_f0 :: Int
        , t2_f1 :: Int
        , t2_f2 :: Int
        , t2_f3 :: Int
        , t2_f4 :: Int
        , t2_f5 :: Int
        , t2_f6 :: Int
        , t2_f7 :: Int
        , t2_f8 :: Int
        , t2_f9 :: Int
        }
#endif
#if GENERIC == 1
  deriving Generic
#endif

data T3 = T3
  { t3_f0 :: Int
  , t3_f1 :: Int
  , t3_f2 :: Int
  , t3_f3 :: Int
  , t3_f4 :: Int
  , t3_f5 :: Int
  , t3_f6 :: Int
  , t3_f7 :: Int
  , t3_f8 :: Int
  , t3_f9 :: Int
  }
#if MULTIPLE >= 1
  | T31 { t3_f0 :: Int
        , t3_f1 :: Int
        , t3_f2 :: Int
        , t3_f3 :: Int
        , t3_f4 :: Int
        , t3_f5 :: Int
        , t3_f6 :: Int
        , t3_f7 :: Int
        , t3_f8 :: Int
        , t3_f9 :: Int
        }
#endif
#if MULTIPLE >= 2
  | T32 { t3_f0 :: Int
        , t3_f1 :: Int
        , t3_f2 :: Int
        , t3_f3 :: Int
        , t3_f4 :: Int
        , t3_f5 :: Int
        , t3_f6 :: Int
        , t3_f7 :: Int
        , t3_f8 :: Int
        , t3_f9 :: Int
        }
#endif
#if MULTIPLE >= 3
  | T33 { t3_f0 :: Int
        , t3_f1 :: Int
        , t3_f2 :: Int
        , t3_f3 :: Int
        , t3_f4 :: Int
        , t3_f5 :: Int
        , t3_f6 :: Int
        , t3_f7 :: Int
        , t3_f8 :: Int
        , t3_f9 :: Int
        }
#endif
#if GENERIC == 1
  deriving Generic
#endif

data T4 = T4
  { t4_f0 :: Int
  , t4_f1 :: Int
  , t4_f2 :: Int
  , t4_f3 :: Int
  , t4_f4 :: Int
  , t4_f5 :: Int
  , t4_f6 :: Int
  , t4_f7 :: Int
  , t4_f8 :: Int
  , t4_f9 :: Int
  }
#if MULTIPLE >= 1
  | T41 { t4_f0 :: Int
        , t4_f1 :: Int
        , t4_f2 :: Int
        , t4_f3 :: Int
        , t4_f4 :: Int
        , t4_f5 :: Int
        , t4_f6 :: Int
        , t4_f7 :: Int
        , t4_f8 :: Int
        , t4_f9 :: Int
        }
#endif
#if MULTIPLE >= 2
  | T42 { t4_f0 :: Int
        , t4_f1 :: Int
        , t4_f2 :: Int
        , t4_f3 :: Int
        , t4_f4 :: Int
        , t4_f5 :: Int
        , t4_f6 :: Int
        , t4_f7 :: Int
        , t4_f8 :: Int
        , t4_f9 :: Int
        }
#endif
#if MULTIPLE >= 3
  | T43 { t4_f0 :: Int
        , t4_f1 :: Int
        , t4_f2 :: Int
        , t4_f3 :: Int
        , t4_f4 :: Int
        , t4_f5 :: Int
        , t4_f6 :: Int
        , t4_f7 :: Int
        , t4_f8 :: Int
        , t4_f9 :: Int
        }
#endif
#if GENERIC == 1
  deriving Generic
#endif

data T5 = T5
  { t5_f0 :: Int
  , t5_f1 :: Int
  , t5_f2 :: Int
  , t5_f3 :: Int
  , t5_f4 :: Int
  , t5_f5 :: Int
  , t5_f6 :: Int
  , t5_f7 :: Int
  , t5_f8 :: Int
  , t5_f9 :: Int
  }
#if MULTIPLE >= 1
  | T51 { t5_f0 :: Int
        , t5_f1 :: Int
        , t5_f2 :: Int
        , t5_f3 :: Int
        , t5_f4 :: Int
        , t5_f5 :: Int
        , t5_f6 :: Int
        , t5_f7 :: Int
        , t5_f8 :: Int
        , t5_f9 :: Int
        }
#endif
#if MULTIPLE >= 2
  | T52 { t5_f0 :: Int
        , t5_f1 :: Int
        , t5_f2 :: Int
        , t5_f3 :: Int
        , t5_f4 :: Int
        , t5_f5 :: Int
        , t5_f6 :: Int
        , t5_f7 :: Int
        , t5_f8 :: Int
        , t5_f9 :: Int
        }
#endif
#if MULTIPLE >= 3
  | T53 { t5_f0 :: Int
        , t5_f1 :: Int
        , t5_f2 :: Int
        , t5_f3 :: Int
        , t5_f4 :: Int
        , t5_f5 :: Int
        , t5_f6 :: Int
        , t5_f7 :: Int
        , t5_f8 :: Int
        , t5_f9 :: Int
        }
#endif
#if GENERIC == 1
  deriving Generic
#endif

#if GENERIC == 1

x_t0_f0 :: T0 -> Int
x_t0_f0 = view $ xfield @"t0_f0"

x_t0_f1 :: T0 -> Int
x_t0_f1 = view $ xfield @"t0_f1"

x_t0_f2 :: T0 -> Int
x_t0_f2 = view $ xfield @"t0_f2"

x_t0_f3 :: T0 -> Int
x_t0_f3 = view $ xfield @"t0_f3"

x_t0_f4 :: T0 -> Int
x_t0_f4 = view $ xfield @"t0_f4"

x_t0_f5 :: T0 -> Int
x_t0_f5 = view $ xfield @"t0_f5"

x_t0_f6 :: T0 -> Int
x_t0_f6 = view $ xfield @"t0_f6"

x_t0_f7 :: T0 -> Int
x_t0_f7 = view $ xfield @"t0_f7"

x_t0_f8 :: T0 -> Int
x_t0_f8 = view $ xfield @"t0_f8"

x_t0_f9 :: T0 -> Int
x_t0_f9 = view $ xfield @"t0_f9"

----

x_t1_f0 :: T1 -> Int
x_t1_f0 = view $ xfield @"t1_f0"

x_t1_f1 :: T1 -> Int
x_t1_f1 = view $ xfield @"t1_f1"

x_t1_f2 :: T1 -> Int
x_t1_f2 = view $ xfield @"t1_f2"

x_t1_f3 :: T1 -> Int
x_t1_f3 = view $ xfield @"t1_f3"

x_t1_f4 :: T1 -> Int
x_t1_f4 = view $ xfield @"t1_f4"

x_t1_f5 :: T1 -> Int
x_t1_f5 = view $ xfield @"t1_f5"

x_t1_f6 :: T1 -> Int
x_t1_f6 = view $ xfield @"t1_f6"

x_t1_f7 :: T1 -> Int
x_t1_f7 = view $ xfield @"t1_f7"

x_t1_f8 :: T1 -> Int
x_t1_f8 = view $ xfield @"t1_f8"

x_t1_f9 :: T1 -> Int
x_t1_f9 = view $ xfield @"t1_f9"

----

x_t2_f0 :: T2 -> Int
x_t2_f0 = view $ xfield @"t2_f0"

x_t2_f1 :: T2 -> Int
x_t2_f1 = view $ xfield @"t2_f1"

x_t2_f2 :: T2 -> Int
x_t2_f2 = view $ xfield @"t2_f2"

x_t2_f3 :: T2 -> Int
x_t2_f3 = view $ xfield @"t2_f3"

x_t2_f4 :: T2 -> Int
x_t2_f4 = view $ xfield @"t2_f4"

x_t2_f5 :: T2 -> Int
x_t2_f5 = view $ xfield @"t2_f5"

x_t2_f6 :: T2 -> Int
x_t2_f6 = view $ xfield @"t2_f6"

x_t2_f7 :: T2 -> Int
x_t2_f7 = view $ xfield @"t2_f7"

x_t2_f8 :: T2 -> Int
x_t2_f8 = view $ xfield @"t2_f8"

x_t2_f9 :: T2 -> Int
x_t2_f9 = view $ xfield @"t2_f9"

----

x_t3_f0 :: T3 -> Int
x_t3_f0 = view $ xfield @"t3_f0"

x_t3_f1 :: T3 -> Int
x_t3_f1 = view $ xfield @"t3_f1"

x_t3_f2 :: T3 -> Int
x_t3_f2 = view $ xfield @"t3_f2"

x_t3_f3 :: T3 -> Int
x_t3_f3 = view $ xfield @"t3_f3"

x_t3_f4 :: T3 -> Int
x_t3_f4 = view $ xfield @"t3_f4"

x_t3_f5 :: T3 -> Int
x_t3_f5 = view $ xfield @"t3_f5"

x_t3_f6 :: T3 -> Int
x_t3_f6 = view $ xfield @"t3_f6"

x_t3_f7 :: T3 -> Int
x_t3_f7 = view $ xfield @"t3_f7"

x_t3_f8 :: T3 -> Int
x_t3_f8 = view $ xfield @"t3_f8"

x_t3_f9 :: T3 -> Int
x_t3_f9 = view $ xfield @"t3_f9"

----

x_t4_f0 :: T4 -> Int
x_t4_f0 = view $ xfield @"t4_f0"

x_t4_f1 :: T4 -> Int
x_t4_f1 = view $ xfield @"t4_f1"

x_t4_f2 :: T4 -> Int
x_t4_f2 = view $ xfield @"t4_f2"

x_t4_f3 :: T4 -> Int
x_t4_f3 = view $ xfield @"t4_f3"

x_t4_f4 :: T4 -> Int
x_t4_f4 = view $ xfield @"t4_f4"

x_t4_f5 :: T4 -> Int
x_t4_f5 = view $ xfield @"t4_f5"

x_t4_f6 :: T4 -> Int
x_t4_f6 = view $ xfield @"t4_f6"

x_t4_f7 :: T4 -> Int
x_t4_f7 = view $ xfield @"t4_f7"

x_t4_f8 :: T4 -> Int
x_t4_f8 = view $ xfield @"t4_f8"

x_t4_f9 :: T4 -> Int
x_t4_f9 = view $ xfield @"t4_f9"

----

x_t5_f0 :: (Int -> Int) -> T5 -> T5
x_t5_f0 = over $ xfield @"t5_f0"

x_t5_f1 :: (Int -> Int) -> T5 -> T5
x_t5_f1 = over $ xfield @"t5_f1"

x_t5_f2 :: (Int -> Int) -> T5 -> T5
x_t5_f2 = over $ xfield @"t5_f2"

x_t5_f3 :: (Int -> Int) -> T5 -> T5
x_t5_f3 = over $ xfield @"t5_f3"

x_t5_f4 :: (Int -> Int) -> T5 -> T5
x_t5_f4 = over $ xfield @"t5_f4"

x_t5_f5 :: (Int -> Int) -> T5 -> T5
x_t5_f5 = over $ xfield @"t5_f5"

x_t5_f6 :: (Int -> Int) -> T5 -> T5
x_t5_f6 = over $ xfield @"t5_f6"

x_t5_f7 :: (Int -> Int) -> T5 -> T5
x_t5_f7 = over $ xfield @"t5_f7"

x_t5_f8 :: (Int -> Int) -> T5 -> T5
x_t5_f8 = over $ xfield @"t5_f8"

x_t5_f9 :: (Int -> Int) -> T5 -> T5
x_t5_f9 = over $ xfield @"t5_f9"

#else

makeFieldLabelsWith noPrefixFieldLabels ''T0
makeFieldLabelsWith noPrefixFieldLabels ''T1
makeFieldLabelsWith noPrefixFieldLabels ''T2
makeFieldLabelsWith noPrefixFieldLabels ''T3
makeFieldLabelsWith noPrefixFieldLabels ''T4
makeFieldLabelsWith noPrefixFieldLabels ''T5

x_t0_f0 :: T0 -> Int
x_t0_f0 = view #t0_f0

x_t0_f1 :: T0 -> Int
x_t0_f1 = view #t0_f1

x_t0_f2 :: T0 -> Int
x_t0_f2 = view #t0_f2

x_t0_f3 :: T0 -> Int
x_t0_f3 = view #t0_f3

x_t0_f4 :: T0 -> Int
x_t0_f4 = view #t0_f4

x_t0_f5 :: T0 -> Int
x_t0_f5 = view #t0_f5

x_t0_f6 :: T0 -> Int
x_t0_f6 = view #t0_f6

x_t0_f7 :: T0 -> Int
x_t0_f7 = view #t0_f7

x_t0_f8 :: T0 -> Int
x_t0_f8 = view #t0_f8

x_t0_f9 :: T0 -> Int
x_t0_f9 = view #t0_f9

----

x_t1_f0 :: T1 -> Int
x_t1_f0 = view #t1_f0

x_t1_f1 :: T1 -> Int
x_t1_f1 = view #t1_f1

x_t1_f2 :: T1 -> Int
x_t1_f2 = view #t1_f2

x_t1_f3 :: T1 -> Int
x_t1_f3 = view #t1_f3

x_t1_f4 :: T1 -> Int
x_t1_f4 = view #t1_f4

x_t1_f5 :: T1 -> Int
x_t1_f5 = view #t1_f5

x_t1_f6 :: T1 -> Int
x_t1_f6 = view #t1_f6

x_t1_f7 :: T1 -> Int
x_t1_f7 = view #t1_f7

x_t1_f8 :: T1 -> Int
x_t1_f8 = view #t1_f8

x_t1_f9 :: T1 -> Int
x_t1_f9 = view #t1_f9

----

x_t2_f0 :: T2 -> Int
x_t2_f0 = view #t2_f0

x_t2_f1 :: T2 -> Int
x_t2_f1 = view #t2_f1

x_t2_f2 :: T2 -> Int
x_t2_f2 = view #t2_f2

x_t2_f3 :: T2 -> Int
x_t2_f3 = view #t2_f3

x_t2_f4 :: T2 -> Int
x_t2_f4 = view #t2_f4

x_t2_f5 :: T2 -> Int
x_t2_f5 = view #t2_f5

x_t2_f6 :: T2 -> Int
x_t2_f6 = view #t2_f6

x_t2_f7 :: T2 -> Int
x_t2_f7 = view #t2_f7

x_t2_f8 :: T2 -> Int
x_t2_f8 = view #t2_f8

x_t2_f9 :: T2 -> Int
x_t2_f9 = view #t2_f9

----

x_t3_f0 :: T3 -> Int
x_t3_f0 = view #t3_f0

x_t3_f1 :: T3 -> Int
x_t3_f1 = view #t3_f1

x_t3_f2 :: T3 -> Int
x_t3_f2 = view #t3_f2

x_t3_f3 :: T3 -> Int
x_t3_f3 = view #t3_f3

x_t3_f4 :: T3 -> Int
x_t3_f4 = view #t3_f4

x_t3_f5 :: T3 -> Int
x_t3_f5 = view #t3_f5

x_t3_f6 :: T3 -> Int
x_t3_f6 = view #t3_f6

x_t3_f7 :: T3 -> Int
x_t3_f7 = view #t3_f7

x_t3_f8 :: T3 -> Int
x_t3_f8 = view #t3_f8

x_t3_f9 :: T3 -> Int
x_t3_f9 = view #t3_f9

----

x_t4_f0 :: T4 -> Int
x_t4_f0 = view #t4_f0

x_t4_f1 :: T4 -> Int
x_t4_f1 = view #t4_f1

x_t4_f2 :: T4 -> Int
x_t4_f2 = view #t4_f2

x_t4_f3 :: T4 -> Int
x_t4_f3 = view #t4_f3

x_t4_f4 :: T4 -> Int
x_t4_f4 = view #t4_f4

x_t4_f5 :: T4 -> Int
x_t4_f5 = view #t4_f5

x_t4_f6 :: T4 -> Int
x_t4_f6 = view #t4_f6

x_t4_f7 :: T4 -> Int
x_t4_f7 = view #t4_f7

x_t4_f8 :: T4 -> Int
x_t4_f8 = view #t4_f8

x_t4_f9 :: T4 -> Int
x_t4_f9 = view #t4_f9

----

x_t5_f0 :: (Int -> Int) -> T5 -> T5
x_t5_f0 = over #t5_f0

x_t5_f1 :: (Int -> Int) -> T5 -> T5
x_t5_f1 = over #t5_f1

x_t5_f2 :: (Int -> Int) -> T5 -> T5
x_t5_f2 = over #t5_f2

x_t5_f3 :: (Int -> Int) -> T5 -> T5
x_t5_f3 = over #t5_f3

x_t5_f4 :: (Int -> Int) -> T5 -> T5
x_t5_f4 = over #t5_f4

x_t5_f5 :: (Int -> Int) -> T5 -> T5
x_t5_f5 = over #t5_f5

x_t5_f6 :: (Int -> Int) -> T5 -> T5
x_t5_f6 = over #t5_f6

x_t5_f7 :: (Int -> Int) -> T5 -> T5
x_t5_f7 = over #t5_f7

x_t5_f8 :: (Int -> Int) -> T5 -> T5
x_t5_f8 = over #t5_f8

x_t5_f9 :: (Int -> Int) -> T5 -> T5
x_t5_f9 = over #t5_f9

#endif

#elif LENS == 0

data T1
  = T10 Int
  | T11 Int
  | T12 Int
  | T13 Int
  | T14 Int
  | T15 Int
  | T16 Int
  | T17 Int
  | T18 Int
  | T19 Int
#if GENERIC == 1
  deriving Generic
#endif

data T2
  = T20 Int
  | T21 Int
  | T22 Int
  | T23 Int
  | T24 Int
  | T25 Int
  | T26 Int
  | T27 Int
  | T28 Int
  | T29 Int
#if GENERIC == 1
  deriving Generic
#endif

data T3
  = T30 Int
  | T31 Int
  | T32 Int
  | T33 Int
  | T34 Int
  | T35 Int
  | T36 Int
  | T37 Int
  | T38 Int
  | T39 Int
#if GENERIC == 1
  deriving Generic
#endif

data T4
  = T40 Int Int Int
  | T41 Int Int Int
  | T42 Int Int Int
  | T43 Int Int Int
  | T44 Int Int Int
  | T45 Int Int Int
  | T46 Int Int Int
  | T47 Int Int Int
  | T48 Int Int Int
  | T49 Int Int Int
#if GENERIC == 1
  deriving Generic
#endif

data T5
  = T50 Int Int Int
  | T51 Int Int Int
  | T52 Int Int Int
  | T53 Int Int Int
  | T54 Int Int Int
  | T55 Int Int Int
  | T56 Int Int Int
  | T57 Int Int Int
  | T58 Int Int Int
  | T59 Int Int Int
#if GENERIC == 1
  deriving Generic
#endif

#if GENERIC == 1

t_11 :: T1 -> Maybe Int
t_11 = preview $ xctor @"T11"

t_12 :: T1 -> Maybe Int
t_12 = preview $ xctor @"T12"

t_13 :: T1 -> Maybe Int
t_13 = preview $ xctor @"T13"

t_14 :: T1 -> Maybe Int
t_14 = preview $ xctor @"T14"

t_15 :: T1 -> Maybe Int
t_15 = preview $ xctor @"T15"

t_16 :: T1 -> Maybe Int
t_16 = preview $ xctor @"T16"

t_17 :: T1 -> Maybe Int
t_17 = preview $ xctor @"T17"

t_18 :: T1 -> Maybe Int
t_18 = preview $ xctor @"T18"

t_19 :: T1 -> Maybe Int
t_19 = preview $ xctor @"T19"

----------------------------------------

t_21 :: T2 -> Maybe Int
t_21 = preview $ xctor @"T21"

t_22 :: T2 -> Maybe Int
t_22 = preview $ xctor @"T22"

t_23 :: T2 -> Maybe Int
t_23 = preview $ xctor @"T23"

t_24 :: T2 -> Maybe Int
t_24 = preview $ xctor @"T24"

t_25 :: T2 -> Maybe Int
t_25 = preview $ xctor @"T25"

t_26 :: T2 -> Maybe Int
t_26 = preview $ xctor @"T26"

t_27 :: T2 -> Maybe Int
t_27 = preview $ xctor @"T27"

t_28 :: T2 -> Maybe Int
t_28 = preview $ xctor @"T28"

t_29 :: T2 -> Maybe Int
t_29 = preview $ xctor @"T29"

----------------------------------------

t_31 :: T3 -> Maybe Int
t_31 = preview $ xctor @"T31"

t_32 :: T3 -> Maybe Int
t_32 = preview $ xctor @"T32"

t_33 :: T3 -> Maybe Int
t_33 = preview $ xctor @"T33"

t_34 :: T3 -> Maybe Int
t_34 = preview $ xctor @"T34"

t_35 :: T3 -> Maybe Int
t_35 = preview $ xctor @"T35"

t_36 :: T3 -> Maybe Int
t_36 = preview $ xctor @"T36"

t_37 :: T3 -> Maybe Int
t_37 = preview $ xctor @"T37"

t_38 :: T3 -> Maybe Int
t_38 = preview $ xctor @"T38"

t_39 :: T3 -> Maybe Int
t_39 = preview $ xctor @"T39"

----------------------------------------

t_41 :: T4 -> Maybe (Int, Int, Int)
t_41 = preview $ xctor @"T41"

t_42 :: T4 -> Maybe (Int, Int, Int)
t_42 = preview $ xctor @"T42"

t_43 :: T4 -> Maybe (Int, Int, Int)
t_43 = preview $ xctor @"T43"

t_44 :: T4 -> Maybe (Int, Int, Int)
t_44 = preview $ xctor @"T44"

t_45 :: T4 -> Maybe (Int, Int, Int)
t_45 = preview $ xctor @"T45"

t_46 :: T4 -> Maybe (Int, Int, Int)
t_46 = preview $ xctor @"T46"

t_47 :: T4 -> Maybe (Int, Int, Int)
t_47 = preview $ xctor @"T47"

t_48 :: T4 -> Maybe (Int, Int, Int)
t_48 = preview $ xctor @"T48"

t_49 :: T4 -> Maybe (Int, Int, Int)
t_49 = preview $ xctor @"T49"

----------------------------------------

t_51 :: T5 -> Maybe (Int, Int, Int)
t_51 = preview $ xctor @"T51"

t_52 :: T5 -> Maybe (Int, Int, Int)
t_52 = preview $ xctor @"T52"

t_53 :: T5 -> Maybe (Int, Int, Int)
t_53 = preview $ xctor @"T53"

t_54 :: T5 -> Maybe (Int, Int, Int)
t_54 = preview $ xctor @"T54"

t_55 :: T5 -> Maybe (Int, Int, Int)
t_55 = preview $ xctor @"T55"

t_56 :: T5 -> Maybe (Int, Int, Int)
t_56 = preview $ xctor @"T56"

t_57 :: T5 -> Maybe (Int, Int, Int)
t_57 = preview $ xctor @"T57"

t_58 :: T5 -> Maybe (Int, Int, Int)
t_58 = preview $ xctor @"T58"

t_59 :: T5 -> Maybe (Int, Int, Int)
t_59 = preview $ xctor @"T59"

#else

makePrismLabels ''T1
makePrismLabels ''T2
makePrismLabels ''T3
makePrismLabels ''T4
makePrismLabels ''T5

t_11 :: T1 -> Maybe Int
t_11 = preview #_T11

t_12 :: T1 -> Maybe Int
t_12 = preview #_T12

t_13 :: T1 -> Maybe Int
t_13 = preview #_T13

t_14 :: T1 -> Maybe Int
t_14 = preview #_T14

t_15 :: T1 -> Maybe Int
t_15 = preview #_T15

t_16 :: T1 -> Maybe Int
t_16 = preview #_T16

t_17 :: T1 -> Maybe Int
t_17 = preview #_T17

t_18 :: T1 -> Maybe Int
t_18 = preview #_T18

t_19 :: T1 -> Maybe Int
t_19 = preview #_T19

----------------------------------------

t_21 :: T2 -> Maybe Int
t_21 = preview #_T21

t_22 :: T2 -> Maybe Int
t_22 = preview #_T22

t_23 :: T2 -> Maybe Int
t_23 = preview #_T23

t_24 :: T2 -> Maybe Int
t_24 = preview #_T24

t_25 :: T2 -> Maybe Int
t_25 = preview #_T25

t_26 :: T2 -> Maybe Int
t_26 = preview #_T26

t_27 :: T2 -> Maybe Int
t_27 = preview #_T27

t_28 :: T2 -> Maybe Int
t_28 = preview #_T28

t_29 :: T2 -> Maybe Int
t_29 = preview #_T29

----------------------------------------

t_31 :: T3 -> Maybe Int
t_31 = preview #_T31

t_32 :: T3 -> Maybe Int
t_32 = preview #_T32

t_33 :: T3 -> Maybe Int
t_33 = preview #_T33

t_34 :: T3 -> Maybe Int
t_34 = preview #_T34

t_35 :: T3 -> Maybe Int
t_35 = preview #_T35

t_36 :: T3 -> Maybe Int
t_36 = preview #_T36

t_37 :: T3 -> Maybe Int
t_37 = preview #_T37

t_38 :: T3 -> Maybe Int
t_38 = preview #_T38

t_39 :: T3 -> Maybe Int
t_39 = preview #_T39

----------------------------------------

t_41 :: T4 -> Maybe (Int, Int, Int)
t_41 = preview #_T41

t_42 :: T4 -> Maybe (Int, Int, Int)
t_42 = preview #_T42

t_43 :: T4 -> Maybe (Int, Int, Int)
t_43 = preview #_T43

t_44 :: T4 -> Maybe (Int, Int, Int)
t_44 = preview #_T44

t_45 :: T4 -> Maybe (Int, Int, Int)
t_45 = preview #_T45

t_46 :: T4 -> Maybe (Int, Int, Int)
t_46 = preview #_T46

t_47 :: T4 -> Maybe (Int, Int, Int)
t_47 = preview #_T47

t_48 :: T4 -> Maybe (Int, Int, Int)
t_48 = preview #_T48

t_49 :: T4 -> Maybe (Int, Int, Int)
t_49 = preview #_T49

----------------------------------------

t_51 :: T5 -> Maybe (Int, Int, Int)
t_51 = preview #_T51

t_52 :: T5 -> Maybe (Int, Int, Int)
t_52 = preview #_T52

t_53 :: T5 -> Maybe (Int, Int, Int)
t_53 = preview #_T53

t_54 :: T5 -> Maybe (Int, Int, Int)
t_54 = preview #_T54

t_55 :: T5 -> Maybe (Int, Int, Int)
t_55 = preview #_T55

t_56 :: T5 -> Maybe (Int, Int, Int)
t_56 = preview #_T56

t_57 :: T5 -> Maybe (Int, Int, Int)
t_57 = preview #_T57

t_58 :: T5 -> Maybe (Int, Int, Int)
t_58 = preview #_T58

t_59 :: T5 -> Maybe (Int, Int, Int)
t_59 = preview #_T59

#endif

#endif

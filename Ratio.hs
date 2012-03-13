{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances     #-}
module Ratio where


import Data.Ratio

import Numeric.Algebra

import qualified Prelude as P
import Prelude hiding ((+),(*),negate)

--Make Ratio a semi ring in the natural way

instance (Integral a) => Additive (Ratio a) where
    (+) x y = (x1 P.* y2 P.+ y1 P.* x2)%(x2 P.* y2)
        where x1 = numerator x
              x2 = denominator x
              y1 = numerator y
              y2 = denominator y

instance (Integral a) => Abelian (Ratio a)

instance  (Integral a) => Multiplicative (Ratio a) where
    (*) x y = (x1 P.* y1)%(x2 P.* y2)
        where x1 = numerator x
              x2 = denominator x
              y1 = numerator y
              y2 = denominator y

instance (Integral a) => Semiring (Ratio a)

instance (Integral a) => Monoidal (Ratio a) where
    zero = 0%1

instance  (Integral a,Integral b,Semiring b) => LeftModule b (Ratio a) where
    (.*) n d = prod%b
        where a = numerator d
              b = denominator d
              prod = P.sum $ replicate (fromIntegral n) a

instance (Integral a,Integral b,Semiring b) => RightModule b (Ratio a) where
    (*.) d n = n.*d

instance (Integral r) => Group (Ratio r) where
    negate d = (P.negate a)%b
        where  a = numerator d
               b = denominator d

instance (Integral r) => Unital (Ratio r) where
    one = 1%1

instance (Integral r) => Division (Ratio r) where
    recip d = b%a
        where a = numerator d
              b = denominator d

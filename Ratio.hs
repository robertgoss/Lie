module Ratio where


import Data.Ratio

import Numeric.Algebra

import qualified Prelude as P

--Make Ratio a semi ring in the natural way

instance (Additive r,P.Integral r) => Additive (Ratio r) where
    (+) a b = a+b

instance (Additive r,P.Integral r) => Abelian (Ratio r)

instance (Additive r,P.Integral r) => Multiplicative (Ratio r) where
    (*) a b = a*b

instance (Semiring a,P.Integral a) => Semiring (Ratio a)

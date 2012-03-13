{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances     #-}
module Octonion(
               ) where

import Ratio
import Data.Ratio

import Data.Word

import Numeric.Algebra
import Numeric.Algebra.Quaternion
import qualified Prelude as P
import Prelude hiding ((+),(*),(-),negate,recip)


data Octonion a = Oct a a a a a a a a
                  deriving (Eq,Read,Ord)

show' n txt
      | n/=zero = show n ++ txt
      | otherwise = ""

instance (Monoidal a,Show a,Eq a) => P.Show (Octonion a) where
    show o@(Oct i0 i1 i2 i3 i4 i5 i6 i7)
         | o==zero ="0"
         | otherwise = show' i0 "i0"
                       ++show' i1 "i1"
                       ++show' i2 "i2"
                       ++show' i3 "i3"
                       ++show' i4 "i4"
                       ++show' i5 "i5"
                       ++show' i6 "i6"
                       ++show' i7 "i7"

instance (Group a,Multiplicative a) => Multiplicative (Octonion a) where
    (*) (Oct i0 i1 i2 i3 i4 i5 i6 i7) (Oct j0 j1 j2 j3 j4 j5 j6 j7) = Oct k0 k1 k2 k3 k4 k5 k6 k7
        where k0 = i0*j0-i1*j1-i2*j2-i3*j3-i4*j4-i5*j5-i6*j6-i7*j7
              k1 = i0*j1+i1*j0+i2*j3-i3*j2+i4*j5-i5*j4-i6*j7+i7*j6
              k2 = i0*j2+i2*j0-i1*j3+i3*j1+i4*j6-i6*j4+i5*j7-i7*j5
              k3 = i0*j3+i3*j0+i1*j2-i2*j1+i4*j7-i7*j4-i5*j6+i6*j5
              k4 = i0*j4+i4*j0-i1*j5+i5*j1-i2*j6+i6*j2+i3*j7-i7*j3
              k5 = i0*j5+i5*j0+i1*j4-i4*j1-i2*j7+i7*j2+i3*j6-i6*j3
              k6 = i0*j6+i6*j0+i1*j6-i6*j1+i2*j4-i6*j2-i3*j5+i5*j3
              k7 = i0*j7+i7*j0-i1*j6+i6*j1+i2*j5-i5*j2+i3*j4-i4*j3

instance (Additive a) => Additive (Octonion a) where
    (+) (Oct i0 i1 i2 i3 i4 i5 i6 i7) (Oct j0 j1 j2 j3 j4 j5 j6 j7) = Oct (i0+j0) (i1+j1) 
                                                                      (i2+j2) (i3+j3)
                                                                      (i4+j4) (i5+j5)
                                                                      (i6+j6) (i7+j7)

instance (Monoidal a) => Monoidal (Octonion a) where
    zero = Oct zero zero zero zero zero zero zero zero

instance (Group a) => Group (Octonion a) where
    negate (Oct i0 i1 i2 i3 i4 i5 i6 i7) = Oct (negate i0) (negate i1) (negate i2) 
                                           (negate i3) (negate i4) (negate i5) 
                                           (negate i6) (negate i7)

instance (Abelian a) => Abelian (Octonion a)



instance (LeftModule r a) => LeftModule r (Octonion a) where
    (.*) s (Oct i0 i1 i2 i3 i4 i5 i6 i7) = Oct (s .* i0) (s .* i1) (s .* i2) (s .* i3) 
                                                    (s .* i4) (s .* i5) (s .* i6) (s .* i7)

instance (RightModule r a) => RightModule r (Octonion a) where
    (*.) (Oct i0 i1 i2 i3 i4 i5 i6 i7) s = Oct (i0 *. s) (i1 *. s) (i2 *. s) (i3 *. s) 
                                                    (i4 *. s) (i5 *. s) (i6 *. s) (i7 *. s)

instance (Group a,Multiplicative a) => InvolutiveMultiplication (Octonion a) where
    adjoint (Oct i0 i1 i2 i3 i4 i5 i6 i7) = Oct i0 (negate i1) (negate i2)
                                            (negate i3) (negate i4) (negate i5)
                                            (negate i6) (negate i7)

instance (Group a,Unital a) => Unital (Octonion a) where
    one = Oct one zero zero zero zero zero zero zero

instance (Semiring r,Group r) => Semiring (Octonion r)

instance (Division r,Semiring r,Group r) => Division (Octonion r) where
    recip (Oct i0 i1 i2 i3 i4 i5 i6 i7) = Oct ((negate r)*i0) (r*i1) (r*i2) (r*i3)  (r*i4)
                                           (r*i5)  (r*i6)  (r*i7)
        where r = negate $ recip $ (i0*i0) + (i1*i1) + (i2*i2) + (i3*i3)
                                 + (i4*i4) + (i5*i5) + (i6*i6) + (i7*i7)

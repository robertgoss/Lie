{-# LANGUAGE TemplateHaskell #-}
module RootTest(tests) where

--Test Framework code
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Root
import Data.Ratio
import Control.Monad

--Useful
sameDim a b = dim a == dim b
notZero = not . isZero
isPositive r = r == (root $ coeff r)

--Hunit Tests
case_zero = isZero (root [0,0,0,0,0]) @?= True
csae_notZero = notZero (root [1,0,0,0]) @?= False

case_reflect1 = (root [3%10,1%5,-7%5,2%5]) @?= 
                reflect (root [1,-1,2,3]) (root [1%2,0,-1,1]) 

--QuickCheck Properties


--Mul Unit

prop_reflect_doub p r = cond ==> r == (reflect p $ reflect p r)
    where types = (r::Root,p::Root)
          cond = (notZero p) && (sameDim p r)

prop_root_pos r = isPositive r
    where types = r::Root

prop_add_pos a b = isPositive (a `rootSum` b)
    where types = (a::Root,b::Root)

prop_reflect_pos p r = cond ==> isPositive $ reflect p r
    where types = (r::Root,p::Root)
          cond = (notZero p) && (sameDim p r)

--Generated Main
main = $(defaultMainGenerator)
tests = $(testGroupGenerator)

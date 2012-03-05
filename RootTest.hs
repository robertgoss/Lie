{-# LANGUAGE TemplateHaskell #-}
module RootTest where

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

--Hunit Tests
case_zero = isZero (root [0,0,0,0,0]) @?= True
csae_notZero = notZero (root [1,0,0,0]) @?= False

case_reflect1 = (root [3%10,1%5,-7%5,2%5]) @?= 
                reflect (root [1,-1,2,3]) (root [1%2,0,-1,1]) 

--QuickCheck Properties


--Mul Unit
prop_mulUnit r = (1 `mul` r) == r
    where types = r::Root

prop_reflect p r = cond ==> r == (reflect p $ reflect p r)
    where types = (r::Root,p::Root)
          cond = (notZero p) && (sameDim p r)



--Generated Main
main = $(defaultMainGenerator)
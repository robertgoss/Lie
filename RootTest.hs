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

--Hunit Tests


--QuickCheck Properties
sameDim a b = dim a == dim b
notZero = not . isZero


--Mul Unit
prop_mulUnit r = (1 `mul` r) == r
    where types = r::Root

prop_reflect p r = cond ==> r == (reflect p $ reflect p r)
    where types = (r::Root,p::Root)
          cond = (notZero p) && (sameDim p r)



--Generated Main
main = $(defaultMainGenerator)
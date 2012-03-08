{-# LANGUAGE TemplateHaskell #-}
module RootTest where

--Test Framework code
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (classify)

import SemiSimple
import RootSystem

import Data.List(sort)

--Useful


eq :: SemiSimple -> SemiSimple -> Bool
eq x y = (stdForm x) == (stdForm y)

--Hunit Tests
 

--QuickCheck Properties
--Added speed testing precond 
prop_classify s = (rank s) < 23 ==> eq s (classify $ rootSystem s)
    where types = s::SemiSimple

prop_basic s = True
    where types = s::SemiSimple



--Generated Main
main = $(defaultMainGenerator)

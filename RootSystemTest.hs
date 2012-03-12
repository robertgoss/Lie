{-# LANGUAGE TemplateHaskell #-}
module RootSystemTest(tests) where

--Test Framework code
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import RootSystem
import Root
import Data.Ratio
import Control.Monad
import Data.List(group,sort)
--Useful
reduced rs = (roots rs) == (map head $ group $ sort $ roots rs)

--Hunit Tests


--QuickCheck Properties

prop_reduced rs = reduced rs
    where types = rs::RootSystem



--Generated Main
main = $(defaultMainGenerator)
tests = $(testGroupGenerator)

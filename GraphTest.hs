{-# LANGUAGE TemplateHaskell #-}
module GraphTest(tests) where

--Test Framework code
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Graph

import Data.List(sort)

--Useful


--Hunit Tests

case_1 = (map sort $ conn_comp es vs) @?= res 
         where es = [(1,3),(2,4),(2,5),(4,5),(6,2),(6,5)] 
               vs = [1,2,3,4,5,6] 
               res = [[1,3],[2,4,5,6]]

--QuickCheck Properties



--Generated Main
main = $(defaultMainGenerator)
tests = $(testGroupGenerator)

{-# LANGUAGE TemplateHaskell #-}
module RootTest where

--Test Framework code
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import SortedLists

import Data.List(sort,group)

--Useful
form a = map head $ group $ sort a
mergeSplit' a b = mergeSplit (form a) (form b)

contain a b = all (`elem` b) a
disjoint a b = not $ any (`elem` b) a
--Hunit Tests
case_mergeSplit1 = mergeSplit [1,2,5,7,8] [2,3,5,6,12] @?=
                   ([1,2,3,5,6,7,8,12],[1,7,8])

--QuickCheck Properties

prop_mergeSplit_union1 a b = contain a union
    where (union,added) = mergeSplit' a b
          types = (a::[Int],b::[Int])

prop_mergeSplit_union2 a b = contain b union
    where (union,added) = mergeSplit' a b
          types = (a::[Int],b::[Int])

prop_mergeSplit_union3 a b = contain union (a++b)
    where (union,added) = mergeSplit' a b
          types = (a::[Int],b::[Int])

prop_mergeSplit_added1 a b = contain added a
    where (union,added) = mergeSplit' a b
          types = (a::[Int],b::[Int])

prop_mergeSplit_added2 a b = disjoint added b
    where (union,added) = mergeSplit' a b
          types = (a::[Int],b::[Int])                          


--Generated Main
main = $(defaultMainGenerator)
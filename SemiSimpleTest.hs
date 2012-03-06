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
order' :: [SemiSimple] -> [SemiSimple]
order' ((Product [x]):xs) = order' (x:xs)
order' ((Product []):xs) = order' xs
order' ((Product ys):xs) = (order' ys)++(order' xs)
order' ((B 1):xs) = (A 1):(order' xs)
order' ((C 1):xs) = (A 1):(order' xs)
order' ((D 1):xs) = (A 1):(order' xs)
order' ((D 2):xs) = (A 1):(A 1):(order' xs)
order' ((C 2):xs) = (B 2):(order' xs)
order' ((A 3):xs) = (D 3):(order' xs)
order' (x:xs) = x:(order' xs)
order' [] = []

order s = sort $ order' s


eq :: SemiSimple -> SemiSimple -> Bool
eq x y = (order [x]) == (order [y])

--Hunit Tests
 

--QuickCheck Properties
--Added speed testing precond 
prop_classify s = (rank s) < 23 ==> eq s (classify $ rootSystem s)
    where types = s::SemiSimple

prop_basic s = True
    where types = s::SemiSimple



--Generated Main
main = $(defaultMainGenerator)
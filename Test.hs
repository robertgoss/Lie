{-# LANGUAGE TemplateHaskell #-}
module Test where

--Test Framework code
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import SemiSimpleTest
import RootSystemTest
import RootTest
import SortedListsTest
import GraphTest

main = defaultMain [RootTest.tests,
                    SemiSimpleTest.tests,
                    RootSystemTest.tests,
                    SortedListsTest.tests,
                    GraphTest.tests]

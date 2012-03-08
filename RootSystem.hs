module RootSystem (
                   RootSystem,
                   simpleRoots,
                   rootSystem,
                   orbit,
                   roots,
                   action,
                   generate,
                   RootSystem.intersect
                  ) where

import Root
import Data.List(group,sort,intersperse)

import Test.QuickCheck
import Control.Monad

import SortedLists

newtype RootSystem = RootSystem [Root]
    deriving (Eq,Ord)

remdup a = map head $ group $ sort a

rootSystem :: [Root] -> RootSystem
rootSystem rs = RootSystem $ remdup rs

roots :: RootSystem -> [Root]
roots (RootSystem rs) = rs

action :: Root -> RootSystem -> RootSystem
action r (RootSystem rs) = RootSystem $ sort $ map (reflect r) rs

orbit :: [Root] -> RootSystem -> [RootSystem]
orbit roots rs = map (`action` rs) roots


simpleRoots :: RootSystem -> [Root]
simpleRoots (RootSystem rs) = filter (`notElem` sums) rs
    where sums = remdup [rootSum a b | a<-rs,b<-rs]

generate' :: [Root] -> [Root] -> [Root] -> [Root]
generate' rs cur []  = cur
generate' rs cur add = generate' rs union added
    where new = remdup [reflect p r | p<-rs,r<-add]
          (union,added) = mergeSplit new cur

generate :: [Root] -> RootSystem
generate rs = RootSystem $ generate' rs [] rs

intersect :: RootSystem -> RootSystem -> RootSystem
intersect (RootSystem xs) (RootSystem ys) = RootSystem $ SortedLists.intersect xs ys

instance Arbitrary RootSystem where
    arbitrary = liftM rootSystem arbitrary

instance Show RootSystem where
    show (RootSystem rs) = concat $ intercalate "\n" (map show rs)

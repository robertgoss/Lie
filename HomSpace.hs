module HomSpace(
                orbits
                ) where

import SortedLists
import RootSystem
import Root

import Data.List(sort,group,foldl1')
import SemiSimple as S

remdup a = map head $ group $ sort a

type RootSubSystem = RootSystem

orbits'' :: [Root] -> [RootSystem] -> [RootSystem] -> [RootSystem]
orbits'' rs cur [] = cur
orbits'' rs cur add = orbits'' rs union added
    where new = remdup $ foldl1' (++) $ map (orbit rs) add
          (union,added) = mergeSplit new cur

orbits' :: [Root] -> RootSystem -> [RootSystem]
orbits' rs h = orbits'' rs [] [h]

orbits :: RootSystem -> RootSubSystem -> RootSubSystem -> [RootSystem]
orbits g h k =  map (intersect k) $ orbits' (roots g) h

extend :: Root -> Root
extend r = root $ (coeff r) ++ [0]

e6' = S.rootSystem S.E6
e6 = RootSystem.rootSystem $ map extend $ roots e6'

spin12 = generate $  map root $ [[0,1,-1,0,0,0,0,0],
                                 [0,1,1,0,0,0,0,0],
                                 [0,0,1,-1,0,0,0,0],
                                 [0,0,0,1,-1,0,0,0],
                                 [0,0,0,0,1,-1,0,0],
                                 [0,0,0,0,0,1,-1,0]]
e7 = S.rootSystem S.E7

subsystem :: RootSystem -> RootSystem -> Bool
subsystem r1 r2 = all (`elem` roots r2) (roots r1)
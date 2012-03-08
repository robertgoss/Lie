module HomSpace(
                orbits
                ) where

import SortedLists
import RootSystem as RS
import Root

import Data.List(sort,group,foldl1')
import SemiSimple as S

import Data.Ratio

remdup a = map head $ group $ sort a

type RootSubSystem = RootSystem

orbits'' :: [Root] -> [RootSystem] -> [RootSystem] -> [RootSystem]
orbits'' rs cur [] = cur
orbits'' rs cur add =   orbits'' rs union added
    where new = remdup $ foldl1' (++) $ map (orbit rs) add
          (union,added) = mergeSplit new cur


orbits' :: [Root] -> RootSystem -> [RootSystem]
orbits' rs h = orbits'' rs [] [h]

orbits :: RootSystem -> RootSubSystem -> RootSubSystem -> [RootSystem]
orbits g h k = map (RS.intersect k) $  orbits' (roots g) h

orbitTypes :: RootSystem -> RootSubSystem -> RootSubSystem -> [SemiSimple]
orbitTypes g h k = map head $ group.sort $ map (stdForm.classify) $ orbits g h k

extend :: Root -> Root
extend r = root $ (coeff r) ++ [0]

subsystem :: RootSystem -> RootSystem -> Bool
subsystem r1 r2 = all (`elem` roots r2) (roots r1)

e7 = S.rootSystem S.E7
e6 = generate $ map root  [[1%2,1%2,-1%2,-1%2,-1%2,-1%2,-1%2,-1%2],[1%2,-1%2,-1%2,-1%2,-1%2,1%2,1%2,1%2],[0,-1,1,0,0,0,0,0],[0,1,1,0,0,0,0,0],[0,0,-1,1,0,0,0,0],[0,0,0,-1,1,0,0,0]]
sp1spin12 =  generate $ map root [[0,1,1,0,0,0,0,0],[0,-1,1,0,0,0,0,0],[0,0,-1,1,0,0,0,0],[0,0,0,-1,1,0,0,0],[0,0,0,0,-1,1,0,0],[0,0,0,0,0,0,1,1]]

f4 = generate $  map root $[[1%2,-1%2,-1%2,-1%2],[0,1,0,0],[0,1,-1,0],[0,0,1,-1]]
spin9 = generate $ map root $ [[1,-1,0,0],[0,1,0,0],[0,1,-1,0],[0,0,1,-1]]
sp3 = generate $ map root $ [[1%2,-1%2,-1%2,-1%2],[0,1,0,0],[0,1,-1,0]]

e8 = S.rootSystem S.E8
spin16 = S.rootSystem (S.D 8)

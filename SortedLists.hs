module SortedLists(
                   mergeSplit,
                   intersect
                  ) where

mergeSplit' :: (Eq a,Ord a) => [a] -> [a] -> ([a],[a]) -> ([a],[a])
mergeSplit' old [] (union,added) = ((reverse union)++old,reverse added)
mergeSplit' [] new (union,added) = ((reverse union)++new,(reverse added)++new)
mergeSplit' (o:old) (n:new) (union,added)
    | o<n = mergeSplit' old (n:new) (o:union,added)
    | o>n = mergeSplit' (o:old) new (n:union,n:added)
    | o==n = mergeSplit' old new (o:union,added)

mergeSplit :: (Eq a,Ord a) => [a] -> [a] -> ([a],[a])
mergeSplit new old = mergeSplit' old new ([],[])

intersect :: (Eq a,Ord a)  => [a] -> [a] -> [a]
intersect [] ys = []
intersect xs [] = []
intersect (x:xs) (y:ys)
    | x==y = x:(intersect xs ys)
    | x<y = intersect xs (y:ys)
    | otherwise = intersect (x:xs) ys

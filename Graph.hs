module Graph(
             conn_comp
             ) where

import Data.Maybe
import Data.List

edgeOf :: (Eq a) => (a,a) -> [a] -> Bool
edgeOf (x,y) v = (x `elem` v) || (y `elem` v)

conn_comp' :: (Eq a) => (a,a)->[[a]]->[[a]]
conn_comp' e vs = conn e vs Nothing
    where conn :: (Eq a) => (a,a) -> [[a]] -> Maybe [a] -> [[a]]
          conn _ [] Nothing = []
          conn _ [] (Just v) = [v]
          conn e (v:vs) Nothing
              | edgeOf e v = conn e vs (Just v)
              | otherwise = v:(conn e vs Nothing)
          conn e (v:vs) (Just w)
              | edgeOf e v = (v++w):vs
              | otherwise = v:(conn e vs (Just w))

conn_comp :: (Eq a) => [(a,a)] -> [a] -> [[a]]
conn_comp es vs = foldr conn_comp' [[v] | v<-vs] es
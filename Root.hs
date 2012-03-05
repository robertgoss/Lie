module Root(
            Root,
            root,
            dot,
            add,
            mul,
            isZero,
            lengthSq,
            dim,
            reflect
           ) where

import Data.Ratio

import Test.QuickCheck
import Control.Monad

newtype Root = Root [Ratio Int]
    deriving (Eq,Show)

root :: [Ratio Int] -> Root
root r = Root r

infixl 8 `dot`
dot :: Root -> Root -> Ratio Int
dot (Root r1) (Root r2) = sum $ map (\(x,y)->x*y) $ zip r1 r2

lengthSq :: Root -> Ratio Int
lengthSq (Root r) = sum $ map (\x->x*x) r

infixl 6 `add`
add :: Root -> Root -> Root
add (Root r1) (Root r2) = Root $ map (\(x,y)->x+y) $ zip r1 r2

infixl 7 `mul`
mul :: Ratio Int -> Root -> Root
mul s (Root r) = Root $ map (s*) r

isZero :: Root -> Bool
isZero r = 0 == lengthSq r


reflect :: Root -> Root -> Root
reflect plane root = root `add` (scale `mul` plane)
    where scale = - 2 * (plane `dot` root) / (lengthSq plane)

dim :: Root -> Int
dim (Root r) = length $ r

--Own root generator used as [Ratio Int] Creates roots too low dim and
--  large in value (overflow problems)

box :: a -> [a]
box a = [a]

extend :: Ratio Int -> Root -> Root
extend w (Root r) = Root (w:r)

instance Arbitrary Root where
  arbitrary = sized root'
      where root' 0 = single
            root' n = oneof [many,single]
            many = oneof $ map (\w->liftM (extend w) arbitrary) wieghts 
            single = oneof $ map (\w->liftM (extend w) empty) wieghts
            empty = return $ Root []
            wieghts = [a%b | a<-[-3..3], b<-[1..3]]
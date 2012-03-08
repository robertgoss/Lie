module Root(
            Root,
            root,
            dot,
            isZero,
            lengthSq,
            dim,
            reflect,
            rootSum,
            coeff
           ) where

import Data.Ratio

import Test.QuickCheck
import Control.Monad

import Data.List(intersperse)

newtype Root = Root [Ratio Int]
    deriving (Eq,Ord)

root :: [Ratio Int] -> Root
root r = Root $ positive' r

coeff :: Root -> [Ratio Int]
coeff (Root r) = r

infixl 8 `dot`
dot :: Root -> Root -> Ratio Int
dot (Root r1) (Root r2) = sum $ zipWith (*) r1 r2

lengthSq :: Root -> Ratio Int
lengthSq (Root r) = sum $ map (\x->x*x) r

infixl 6 `rootSum`
rootSum :: Root -> Root -> Root
rootSum r s = positive (r `add` s)

infixl 6 `add`
add :: Root -> Root -> Root
add (Root r1) (Root r2) = Root ZipWith (+) r1 r2

infixl 7 `mul`
mul :: Ratio Int -> Root -> Root
mul s (Root r) = Root $ map (s*) r

isZero :: Root -> Bool
isZero r = 0 == lengthSq r


reflect :: Root -> Root -> Root
reflect plane root = positive $ root `add` (scale `mul` plane)
    where scale = - 2 * (plane `dot` root) / lengthSq plane

dim :: Root -> Int
dim (Root r) = length r

positive' :: [Ratio Int] -> [Ratio Int]
positive' [] = []
positive' (r:rs)
    | r > 0 = r:rs
    | r < 0 = map ((-1)*) r:rs
    | otherwise = 0: positive' rs

positive :: Root -> Root
positive (Root r) = Root (positive' r)

--Own root generator used as [Ratio Int] Creates roots too low dim and
--  large in value (overflow problems)

box :: a -> [a]
box a = [a]

extend :: Ratio Int -> Root -> Root
extend w (Root r) = root (w:r)

instance Arbitrary Root where
  arbitrary = sized root'
      where root' 0 = single
            root' n = oneof [many,single]
            many = oneof $ map (\w->liftM (extend w) arbitrary) wieghts 
            single = oneof $ map (\w->liftM (extend w) empty) wieghts
            empty = return $ Root []
            wieghts = [a%b | a<-[-3..3], b<-[1..3]]

simple :: Ratio Int -> String
simple q
       | b==1 = show a
       | otherwise = show a ++ "/"++ show b
       where a = numerator q
             b = denominator q

instance Show Root where
    show (Root r) = "[" ++ concat intercalate ( "," $ map simple r) ++ "]"

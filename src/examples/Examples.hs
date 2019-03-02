{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (sum, product)
import GHC.Base hiding (Monoid)
import Data.Foldable hiding (sum, product)

-- start snippet simple-sum-product
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs
-- end snippet simple-sum-product

main :: IO ()
main = putStrLn "Hello world"

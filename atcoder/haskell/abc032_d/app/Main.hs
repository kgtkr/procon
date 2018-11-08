module Main where

import           Control.Monad
import           Data.Array.ST
import           Control.Monad.ST

main :: IO ()
main = do
  (n, w) <- readPair
  list   <- replicateM n readPair
  print $ napsack w list
 where
  readPair = listToTuple2 . fmap read . words <$> getLine
  listToTuple2 [x, y] = (x, y)
  napsack r list = runST $ do
    arr <-
      newArray ((0, 0), (100, 10000)) (-1) :: ST s (STArray s (Int, Int) Int)
    f arr 0 r list
  f arr i r ((v, w) : xs) = do
    x <- readArray arr (i, r)
    if x /= (-1)
      then return x
      else do
        res <- if r < w
          then f arr (i + 1) r xs
          else
            max
            <$> ((+ v) <$> (f arr (i + 1) (r - w) xs))
            <*> (f arr (i + 1) r xs)
        writeArray arr (i, r) res
        return res
  f arr _ _ [] = return 0

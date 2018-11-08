module Main where

import           Control.Monad
import           Data.Array.ST
import           Control.Monad.ST

main :: IO ()
main = do
  (n, w) <- readPair
  list   <- replicateM n readPair
  print $ f w 0 list
  return ()
 where
  readPair = listToTuple2 . fmap read . words <$> getLine
  listToTuple2 [x, y] = (x, y)
  f r i ((v, w) : xs)
    | r < w     = f r (i + 1) xs
    | otherwise = max ((f (r - w) (i + 1) xs) + v) (f r (i + 1) xs)
  f _ _ [] = 0


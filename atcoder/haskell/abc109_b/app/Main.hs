module Main where

import           Data.List
import           Control.Monad

main :: IO ()
main = do
    n    <- readLn
    list <- replicateM n getLine
    putStr
        $ if (length . nub) list
             == length list
             && (siritori ((head . head) list) list)
          then
              "Yes"
          else
              "No"
    return ()

siritori :: Char -> [String] -> Bool
siritori _    []       = True
siritori next (x : xs) = head x == next && siritori (last x) xs

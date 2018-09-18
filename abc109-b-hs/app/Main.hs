module Main where

import           Data.List

main :: IO ()
main = do
    n    <- readLn :: IO Int
    list <- (sequence . take n . repeat) getLine
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

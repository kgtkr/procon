module Main where

main :: IO ()
main = do
    k <- readLn :: IO Int
    print $ (k `div` 2) * ((k `div` 2) + (if k `mod` 2 == 1 then 1 else 0))


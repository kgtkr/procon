module Main where

main :: IO ()
main = do
    [n, k] <- (map read . words) <$> getLine :: IO [Int]
    print $ if k == 2
        then (n `div` 2) ^ 3 + (n `myDiv` 2) ^ 3
        else (n `div` k) ^ 3 + if even k
            then ((n + k `div` 2) `div` k) ^ 3
            else 0
    return ()

myDiv a b = (a + (b - 1)) `div` b

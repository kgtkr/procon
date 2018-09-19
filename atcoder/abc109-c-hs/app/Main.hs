module Main where

main :: IO ()
main = do
    [n, x] <- (map read . words) <$> getLine :: IO [Int]
    list   <- (map read . words) <$> getLine :: IO [Int]
    print $ (foldr gcd 0 . map (subtract x)) list
    return ()

module Main where

main :: IO ()
main = do
    [a, b] <- (map read . words) <$> getLine :: IO [Int]
    putStrLn $ if odd (a * b) then "Yes" else "No"
    return ()

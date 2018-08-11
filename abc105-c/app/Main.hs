module Main where

main :: IO ()
main = do
    n <- readLn
    let s = f "" n
    putStrLn $ s

f :: String -> Int -> String
f "" 0 = "0"
f s  0 = s
f s n | n `mod` 2 == 0 = f ('0' : s) (n `div` (-2))
      | otherwise      = f ('1' : s) ((n - 1) `div` (-2))

main :: IO ()
main = readLn >>= putStrLn . f ""

f :: String -> Int -> String
f "" 0 = "0"
f s  0 = s
f s  n = f (show r ++ s) $ (r - n) `div` 2 where r = abs n `mod` 2

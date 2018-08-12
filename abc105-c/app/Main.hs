main :: IO ()
main = readLn >>= putStrLn . f ""

f :: String -> Int -> String
f "" 0 = "0"
f s  0 = s
f s n | even n    = f ('0' : s) $ n `div` (-2)
      | otherwise = f ('1' : s) $ (n - 1) `div` (-2)

import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve s | x > y     = "Alice"
        | y > x     = "Bob"
        | otherwise = "Draw"
 where
  [a, b] = fmap read (words s) :: [Int]
  x      = if a == 1 then 100 else a
  y      = if b == 1 then 100 else b

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
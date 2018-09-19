import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve s | y >= 0     = "delicious"
        | abs y <= x = "safe"
        | otherwise  = "dangerous"
 where
  [x, a, b] = fmap read (words s) :: [Int]
  y         = a - b

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve s | x > y     = show x
        | otherwise = show y
 where
  [a, b, c, d] = fmap read (words s) :: [Int]
  x            = a * b
  y            = c * d

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
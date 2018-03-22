import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve s | op == "+" = show $ a + b
        | otherwise = show $ a - b
 where
  [x, op, y] = (words s)
  [a, b]     = fmap read [x, y] :: [Int]

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
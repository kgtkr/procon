import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve s | da < db = "A"
        | db < da = "B"
 where
  [x, a, b] = fmap read (words s) :: [Int]
  da        = abs (x - a)
  db        = abs (x - b)

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
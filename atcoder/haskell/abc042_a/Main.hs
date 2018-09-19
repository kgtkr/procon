import Data.Maybe
import Data.List
 
main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))
 
--処理
solve :: String -> String
solve s = if x == [5, 5, 7] then "YES" else "NO"
  where x = sort $ fmap read (words s) :: [Int]
 
trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))
 
trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
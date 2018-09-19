import Data.Maybe
import Data.List
 
main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))
 
--処理
solve :: String -> String
solve s = show $ min (n-x) (x-1)
  where [n,x] = fmap read (words s) :: [Int]
 
trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))
 
trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
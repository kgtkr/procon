import Data.Maybe
import Data.List
 
main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))
 
--処理
solve :: String -> String
solve s = show $ (c*b)*2+(a*c)*2+(a*b)*2
  where [a,b,c] = fmap read (words s) :: [Int]
 
trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))
 
trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
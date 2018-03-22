import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve s = show $ x * (ceiling (n2 / x2))
 where
  [a, b, n] = fmap read (lines s) :: [Integer]
  x         = lcm a b
  x2        = fromInteger x :: Double
  n2        = fromInteger n :: Double

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
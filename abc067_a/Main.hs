import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve s | a `mod` 3 == 0 || b `mod` 3 == 0 || c `mod` 3 == 0 = "Possible"
        | otherwise = "Impossible"
 where
  [a, b] = fmap read (words s) :: [Int]
  c      = a + b

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve s | g x == g y = "Yes"
        | otherwise  = "No"
  where [x, y] = fmap read (words s) :: [Int]

g :: Int -> Int
g x | x == 2                 = 0
    | x `elem` [4, 6, 9, 11] = 1
    | otherwise              = 2

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
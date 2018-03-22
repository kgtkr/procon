import System.Environment
import Data.Maybe
import Data.List
import Control.Exception

main :: IO ()
main = do
  isTest <- getEnv "TEST" `catch` (\(SomeException _) -> return "")
  if isTest == "1"
    then do
      testFile <- readFile "tests"
      putStrLn $ tests testFile
    else do
      input <- getContents
      putStrLn $ trim (solve (trim input))

--処理
solve :: String -> String
solve [c1,c2,c3,_,c4,c5,c6]|c1==c6&&c2==c5&&c3==c4="YES"
                          |otherwise="NO"

tests :: String -> String
tests text = case testParse text of
  Right bodys -> unlines (fmap (\(i, body) -> test i body) (zip [1 ..] bodys))
  Left  msg   -> "Test file parse error:" ++ msg

test :: Int -> (String, String) -> String
test n (input, outputRight)
  | output == outputRight
  = header ++ "Passed"
  | otherwise
  = header
    ++ "Faild\n"
    ++ "Right:"
    ++ show outputRight
    ++ "\nOutput:"
    ++ show output
 where
  header = "test" ++ show n ++ ":"
  output = trim $ solve input

testParse :: String -> Either String [(String, String)]
testParse s = case lines s of
  (sp1:sp2:bodys) -> testBodysParse sp1 sp2 bodys
  _               -> Left "testParse"

testBodysParse
  :: String -> String -> [String] -> Either String [(String, String)]
testBodysParse sp1 sp2 = (traverse . testBodyParse) sp2 . (split sp1)

testBodyParse :: String -> [String] -> Either String (String, String)
testBodyParse sp body = case fmap (trim . unlines) (split sp body) of
  [input, output] -> Right (input, output)
  _               -> Left "testBodyParse"

split :: Eq a => a -> [a] -> [[a]]
split spratar = foldr f [[]]
 where
  f x (p:ps) | x == spratar = [] : p : ps
             | otherwise    = (x : p) : ps
  f _ _ = undefined

trimHead :: String -> String
trimHead = dropWhile (\s -> isJust (elemIndex s [' ', '\t', '\n', '\r']))

trim :: String -> String
trim = reverse . trimHead . reverse . trimHead
main :: IO ()
main = undefined

--処理
solve :: String -> String
solve = undefined

testParse :: String -> Maybe [(String, String)]
testParse s = case lines s of
  (sp1:sp2:bodys) -> testBodysParse sp1 sp2 bodys
  _               -> Nothing

testBodysParse :: String -> String -> [String] -> Maybe [(String, String)]
testBodysParse sp1 sp2 bodys =
  (sequence . fmap (testBodyParse sp2) . split sp1) bodys

testBodyParse :: String -> [String] -> Maybe (String, String)
testBodyParse sp body = case split sp body of
  [input, output] -> Just (unlines input, unlines output)
  _               -> Nothing

split :: Eq a => a -> [a] -> [[a]]
split spratar body = foldr
  (\x y -> if x == spratar then [] : y else let (p:ps) = y in (x : p) : ps)
  [[]]
  body


























































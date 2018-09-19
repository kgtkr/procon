module Main where

import           Control.Monad

main :: IO ()
main = do
    [h, _] <- (map read . words) <$> getLine :: IO [Int]
    list   <- map (map read . words) <$> (replicateM h) getLine :: IO [[Int]]
    let res1 = (map goRight . enumerate2d) list
    let
        result = concat
            [(concatMap fst) res1, (fst . goRight . map (last . snd)) res1]

    putStr
        $ (unlines . (((show . length) result) :) . map
              (unwords . (\((a, b), (c, d)) -> (map show) [a, b, c, d]))
          )
              result

    return ()
goRight
    :: [((Int, Int), Int)] -> ([((Int, Int), (Int, Int))], [((Int, Int), Int)])
goRight []  = ([], [])
goRight [x] = ([], [x])
goRight (a@(aPoint, aValue) : b@(bPoint, _) : xs) = if even aValue
    then mapSnd (a :) (goRight (b : xs))
    else
        let (resMove, resMap) = goRight (mapSnd (+ 1) b : xs)
        in  ((aPoint, bPoint) : resMove, mapSnd (subtract 1) a : resMap)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1 ..]

enumerate2d :: [[a]] -> [[((Int, Int), a)]]
enumerate2d =
    (map (\(y, a) -> (map (\(x, b) -> ((y, x), b)) . enumerate) a) . enumerate)

mapTuple :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
mapTuple f1 f2 (a, b) = (f1 a, f2 b)

mapFst :: (t1 -> a) -> (t1, b) -> (a, b)
mapFst = flip mapTuple id

mapSnd :: (t2 -> b) -> (a, t2) -> (a, b)
mapSnd = mapTuple id

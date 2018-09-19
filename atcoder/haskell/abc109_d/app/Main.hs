module Main where

import           Control.Monad

main :: IO ()
main = do
    [h, _] <- (map read . words) <$> getLine :: IO [Int]
    list   <- map (map read . words) <$> (replicateM h) getLine :: IO [[Int]]
    let res1    = (map goRight . enumerate2d) list
    let list2   = (map snd) res1
    let result1 = (concatMap fst) res1
    let res2    = (goRight . map last) list2
    let result2 = fst res2
    let result  = concat [result1, result2]

    putStr
        $ (unlines . (((show . length) result) :) . map
              (unwords . (\((a, b), (c, d)) -> (map show) [a, b, c, d]))
          )
              result

    return ()
goRight
    :: [((Int, Int), Int)] -> ([((Int, Int), (Int, Int))], [((Int, Int), Int)])
goRight []           = ([], [])
goRight [x         ] = ([], [x])
goRight (a : b : xs) = if even (snd a)
    then let res = goRight (b : xs) in mapSnd (a :) res
    else
        let res = goRight (mapSnd (+ 1) b : xs)
        in  ((fst a, fst b) : (fst res), mapSnd (subtract 1) a : (snd res))

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

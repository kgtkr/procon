module Main where

import           Data.List

main :: IO ()
main = do
    [h, w] <- (map read . words) <$> getLine :: IO [Int]
    list   <-
        (map (map read . words)) <$> (sequence . take h . repeat) getLine :: IO
            [[Int]]
    let res1    = (map goRight . enumerate2d) list
    let list2   = (map snd) res1
    let result1 = (concat . map fst) res1
    let res2    = (map goRight . transpose) list2
    let result2 = (concat . map fst) res2
    let result  = concat [result1, result2]

    putStr
        $ (unlines . (((show . length) result) :) . map
              (unwords . (\((a, b), (c, d)) -> (map show) [a, b, c, d]))
          )
              result

    return ()
goRight
    :: [((Int, Int), Int)] -> ([((Int, Int), (Int, Int))], [((Int, Int), Int)])
goRight list@[_         ] = ([], list)
goRight (     a : b : xs) = if even (snd a)
    then let res = goRight (b : xs) in (fst res, a : (snd res))
    else
        let res = goRight ((fst b, (snd b) + 1) : xs)
        in  ((fst a, fst b) : (fst res), (fst a, (snd a) - 1) : (snd res))

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1 ..]

enumerate2d :: [[a]] -> [[((Int, Int), a)]]
enumerate2d list =
    (map (\(y, a) -> (map (\(x, b) -> ((y, x), b)) . enumerate) a) . enumerate)
        list

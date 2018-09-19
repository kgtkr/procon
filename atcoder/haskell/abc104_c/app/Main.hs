module Main where

import           Control.Monad

main :: IO ()
main = do
    [d, g] <- map read . words <$> getLine
    list   <-
        map ((\[a, b] -> (a, b)) . map read . words) <$> replicateM d getLine :: IO
            [(Int, Int)]

    let
        res =
            (callF . (f g 0) . map (\(a, (b, c)) -> (a, b, c)) . zip
                    [100, 200 ..]
                )
                list
    print res

inf :: Int
inf = 99999999

callF :: (Bool -> Int) -> Int
callF f = min (f True) (f False)

f :: Int -> Int -> [(Int, Int, Int)] -> Bool -> Int
f 0 c []       _     = c
f _ c []       _     = inf
f g c (_ : xs) False = callF $ f g c xs
f g c ((x, y, z) : xs) True | fewG > 0  = callF $ f (max 0 allG) (c + y) xs
                            | otherwise = callF $ f 0 (c + divN) xs
  where
    fewG = g - x * (y - 1)
    allG = g - x * y - z
    divN = g `bigDiv` x

bigDiv :: Int -> Int -> Int
bigDiv a b = (a + b - 1) `div` b

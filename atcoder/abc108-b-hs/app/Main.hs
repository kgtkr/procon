module Main where
import           Data.List
main :: IO ()
main = do
    [x1, y1, x2, y2] <- (map read . words) <$> getLine :: IO [Int]
    putStrLn $ (intercalate " " . map show)
        [x2 - y2 + y1, y2 + x2 - x1, x1 - y2 + y1, y1 + x2 - x1]
    return ()

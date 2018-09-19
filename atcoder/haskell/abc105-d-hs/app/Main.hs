import           Data.Maybe
import           Data.List

main :: IO ()
main = do
    input <- getContents
    let [n, m] : list : _ = map read . words <$> lines input :: [[Int]]
    ( putStrLn
        . show
        . sum
        . map (\n -> n * (n - 1) `div` 2)
        . map length
        . group
        . sort
        . map (`mod` m)
        . scanl (+) 0
        )
        list

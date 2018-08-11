import           Data.Maybe
import           Data.List

main :: IO ()
main = do
    input <- getContents
    let [n, m] : list : _ = map read . words <$> lines input :: [[Int]]
    let f n = (n * (n - 1)) `div` 2
    let
        x =
            (sum . map f . map length . group . sort . map (`mod` m) . scanl
                    (+)
                    0
                )
                list
    putStrLn $ show x

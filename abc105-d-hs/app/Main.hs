import           Data.Maybe
import           Data.List

main :: IO ()
main = do
    input <- getContents
    let l1 : l2 : _ = lines input
    let [n, m]      = (map read . words) l1 :: [Int]
    let list        = (map read . words) l2 :: [Int]
    let mlist       = (map (`mod` m) . scanl1 (+)) list
    let zero        = (length . filter (== 0)) mlist
    let f n = (n * (n - 1)) `div` 2
    let x = zero + (sum . map f . map length . group . sort) mlist
    putStrLn $ show x

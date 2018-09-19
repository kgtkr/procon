module Main where

import           Control.Monad
import Control.Monad.Writer


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

main :: IO ()
main = do
    [h, _] <- (map read . words) <$> getLine
    list   <- map (map read . words) <$> (replicateM h) getLine
    let w=do
            res<-(sequence . map goRight . enumerate2d) list
            (goRight . map last) res
            return ()
    let (_,log)=runWriter w

    putStr
        $(unlines .((show . length .fromDiffList) log :) . map ((unwords . map show) . (\((a, b), (c, d)) -> [a, b, c, d])).fromDiffList)
        log

goRight
    :: [((Int, Int), Int)] -> Writer (DiffList ((Int, Int), (Int, Int))) [((Int, Int), Int)]
goRight []  = return []
goRight [x] = return [x]
goRight (a@(aPoint, aValue) : b@(bPoint, _) : xs) = do
    if even aValue then
        do
            res<-goRight (b : xs)
            return $ a:res
    else
        do
            res<-goRight (fmap (+ 1) b : xs)
            tell $ toDiffList [(aPoint, bPoint)]
            return $ fmap (subtract 1) a : res

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1 ..]

enumerate2d :: [[a]] -> [[((Int, Int), a)]]
enumerate2d =
    (map (\(y, a) -> (map (\(x, b) -> ((y, x), b)) . enumerate) a) . enumerate)

    
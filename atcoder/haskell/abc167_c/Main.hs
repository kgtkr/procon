{-# LANGUAGE TypeApplications #-}

import Data.Maybe
import Data.List
import Control.Monad
import Data.Bifunctor
import Control.Applicative
import Data.Monoid
import Data.Coerce

main :: IO ()
main = do
    [n, m, x] <- fmap (read @Int) . words <$> getLine
    list <- replicateM n $ fromJust . uncons . fmap (read @Int) . words <$> getLine
    print $ solve x list

solve :: Int -> [(Int, [Int])] -> Int
solve x = fromMaybe (-1)
        . foldl1May min
        . map fst
        . filter (all (>= x) . snd)
        . id @[(Int, [Int])]
        . coerce
        . fmap mconcat
        . traverse (: [mempty])
        . id @[(Sum Int, Ap ZipList (Sum Int))]
        . coerce

foldl1May :: (a -> a -> a) -> [a] -> Maybe a
foldl1May f (x:xs) = Just $ foldl f x xs
foldl1May _ _ = Nothing

{-# LANGUAGE TypeApplications #-}

import Data.Maybe
import Data.List
import Control.Monad
import Data.Bifunctor
import Control.Applicative
import Data.Monoid

main :: IO ()
main = do
    [n, m, x] <- fmap (read @Int) . words <$> getLine
    list <- replicateM n $ fromJust . uncons . fmap (read @Int) . words <$> getLine
    print $ (fromMaybe (-1) . foldl1May min . map fst . filter (all (>= x) . take m . snd) . fmap (bimap getSum (fmap getSum . getZipList . getAp)) . fmap mconcat . traverse (: [mempty]) . fmap (bimap Sum (Ap . ZipList . fmap Sum))) list
    pure ()


foldl1May :: (a -> a -> a) -> [a] -> Maybe a
foldl1May f (x:xs) = Just $ foldl f x xs
foldl1May _ _ = Nothing
{-# LANGUAGE TypeApplications, TupleSections #-}

import Control.Monad
import Data.List

main = ((++) <$> (fmap (, 1) <$> ((readLn @ Int) >>= (flip replicateM) getLine)) <*> (fmap (, -1) <$> ((readLn @ Int) >>= (flip replicateM) getLine))) >>= (print . foldl max 0 . fmap (sum . fmap snd) . groupBy ((. fst) . (==) . fst) . sort)

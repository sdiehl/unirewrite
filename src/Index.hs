module Index where

import Data.Data
import Data.Generics.Uniplate.Data

indexed :: (Enum i, Num i, Data on) => on -> [(i, on)]
indexed tree = zip [0..] $ children tree

labeled :: (Enum i, Num i, Data on) => on -> [([i], on)]
labeled tree = ([], tree) : [ (x:xs, tree) | (x, subtree) <- indexed tree,
                                             (xs, tree)   <- labeled subtree ]

module Index where

import Data.Data
import Data.Generics.Uniplate.Data

type Id = [Int]

indexed :: Data on => on -> [(Int, on)]
indexed tree = zip [0..] $ children tree

labeled :: Data on => on -> [(Id, on)]
labeled tree = ([], tree) : [ (x:xs, tree) | (x, subtree) <- indexed tree,
                                             (xs, tree)   <- labeled subtree ]

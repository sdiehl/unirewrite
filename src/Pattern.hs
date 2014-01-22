module Pattern (
  replaceWith,
  replaceWithMap,
  pattern,
  guarded,
  matchCase
) where

import Match
import Data.Data
import Data.Maybe
import qualified Data.Map as Map

import Data.Generics.Uniplate.Data

replaceWith :: (Eq on, Data on) => on -> on -> on -> on
replaceWith x y = transform fn
  where
    fn x0 | x0 == x = y
    fn x0 = x0

replaceWithMap :: (Data a, Ord a) => Map.Map (Maybe a) a -> Maybe a -> Maybe a
replaceWithMap subst = transform fn
  where fn x = Map.lookup x subst

-------------------------------------------------------------------------------
-- Pattern Matching
-------------------------------------------------------------------------------

-- | Apply /expr/ to the /lhs/ of a "function" and rebind the /rhs/ scope if it matches the /lhs/
pattern :: Matchable a => (a, a) -> a -> Maybe a
pattern (lhs, rhs) expr
    | matchq    = Just $ apply subst rhs
    | otherwise = Nothing
  where
    (matchq, subst) = runMatcher lhs expr

-- | Apply /expr/ to the /lhs/ of a "function" and rebind the /rhs/ scope if it matches the /lhs/ and /guard/
-- tests true.
guarded :: (Matchable a, Testable a) => (a, a, a) -> a -> Maybe a
guarded (lhs, rhs, guard) expr
  | matchq =
    if fromJust $ test guard then
      Just $ apply subst rhs
    else
      Nothing
  | otherwise = Nothing
  where
    (matchq, subst) = runMatcher lhs expr

-- | Match a list of patterns binding the rhs if the pattern is matched.
matchCase :: Matchable a => a -> [(a, a)] -> Maybe a
matchCase _ [] = Nothing
matchCase var ((lhs, rhs):xs)
    | matchq    = Just $ apply subst rhs
    | otherwise = matchCase var xs
  where
    (matchq, subst) = runMatcher lhs var

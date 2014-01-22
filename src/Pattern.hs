{-# LANGUAGE NoMonomorphismRestriction #-}

module Pattern (
  replaceWith,
  replaceWithMap,
  pattern,
  gpattern,
  cases,
  gcases,

  toGuarded,
  toGuardeds
) where

import Match
import Data.Data
import qualified Data.Map as Map

import Control.Monad

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
gpattern :: (Matchable a, Testable a) => (a, a, a) -> a -> Maybe a
gpattern (lhs, rhs, grd) expr
    | matchq =
      if testq (apply subst grd) then
        Just $ apply subst rhs
      else
        Nothing
    | otherwise = Nothing
  where
    (matchq, subst) = runMatcher lhs expr

-- | Match a list of patterns.
cases :: Matchable a => a -> [(a, a)] -> Maybe a
cases _ [] = Nothing
cases expr cs = msum [pattern c expr | c <- cs]

-- | Match a list of guarded patterns.
gcases :: (Matchable a, Testable a) => a -> [(a, a, a)] -> Maybe a
gcases _ [] = Nothing
gcases expr cs = msum [gpattern c expr | c <- cs]

toGuarded :: (Matchable a, Testable a) => (a, a) -> (a, a, a)
toGuarded (a, b) = (a, b, true)

toGuardeds :: (Matchable a, Testable a) => [(a, a)] -> [(a, a, a)]
toGuardeds = map toGuarded

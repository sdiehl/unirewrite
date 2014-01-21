{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Match (

  -- * Substitutions
  apply,
  bind,

  -- * Matching
  Subst,
  MatchM,
  emptymatch,
  nomatch,
  donematch,
  runMatcher,

  -- * Match results
  matches,
  matchList,
  matchSubst,

  -- * Classes
  Matchable(..),
  Testable(..)
) where

import Data.Data
import Data.Monoid
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import Data.Generics.Uniplate.Data

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Pattern match monad
type MatchM a b = State (Subst a b) Bool

class (Ord a, Data a) => Matchable a where
  isPattern   :: a -> Bool
  bindPattern :: a -> a
  match       :: a -> a -> MatchM a a

-- Guard conditions
class Testable a where
  test  :: a -> Maybe Bool
  testq :: a -> Bool
  testq x = case test x of
    (Just True) -> True
    _           -> False


-- | Run pattern matcher
runMatcher :: Matchable a => a -> a -> (Bool, Subst a a)
runMatcher a b = runState m emptySubst
  where m = zipMatch' (universe a) (universe b)

zipMatch' :: Matchable a => [a] -> [a] -> MatchM a a
zipMatch' [] [] = donematch
zipMatch' [] _ = nomatch
zipMatch' _ [] = nomatch
zipMatch' (x:xs) (y:ys) = match x y >> zipMatch' xs ys

-------------------------------------------------------------------------------
-- Substution
-------------------------------------------------------------------------------

-- | Substitution
newtype Subst a b = Subst { unSubst :: Map.Map a b }
    deriving Show

fromMap :: Map.Map a b -> Subst a b
fromMap = Subst

toMap :: Subst a b -> Map.Map a b
toMap = unSubst

emptySubst :: Subst a b
emptySubst = fromMap Map.empty

nullSubst :: Subst a b -> Bool
nullSubst = Map.null . toMap

-- (s1 `compose` s2) `apply` t = s1 `apply` (s2 `apply` t).
compose :: (Data a, Ord a, Matchable a) => Subst a a -> Subst a a -> Subst a a
compose s1 s2 = fromMap (Map.unionWith const (apply s1 <$> toMap s2) (toMap s1))

unionSubst :: Matchable a => Subst a b -> Subst a b -> Subst a b
unionSubst s1 s2 = Subst (toMap s1 `Map.union` toMap s2)

-- | Apply matching substition to expression
apply :: Matchable a => Subst a a -> a -> a
apply b = transform go
  where
    go e | isPattern e = Map.findWithDefault e (bindPattern e) (toMap b)
         | otherwise = e

-- | Universal match
emptymatch :: MatchM a b
emptymatch = return True

-- | Fail pattern
nomatch :: MatchM a b
nomatch = return False

-- | End pattern
donematch :: MatchM a b
donematch = return True

-- | Bind pattern to value
bind :: Matchable a => a -> a -> MatchM a a
bind p e = do
  s0 <- get
  let s = toMap s0
  -- If the bound result is identical to previously matched
  -- value then permit the non-linear pattern.
  case Map.lookup p s of
    Just x | x == e    -> return True
           | otherwise -> return False
    Nothing -> do
      put $ fromMap $ Map.insert p e s
      return True

-- | Matching a pattern returning if the pattern matches
matches :: Matchable a => a -> a -> Bool
matches pat expr = fst (runMatcher pat expr)

-- | Match a pattern returning a substitution
matchSubst :: Matchable a => a -> a -> Subst a a
matchSubst pat expr = snd (runMatcher pat expr)

-- | Match a pattern returning the list of substitutions
matchList :: Matchable a => a -> a -> [(a, a)]
matchList pat expr = (Map.toList . toMap) $ matchSubst pat expr


instance Matchable a => Monoid (Subst a a) where
  mempty  = emptySubst
  mappend = compose

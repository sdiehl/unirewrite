module Match (
  match,
  apply,
  matches
) where

import Data.Data
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import Data.Generics.Uniplate.Data

type MatchM a b = State (Subst a b) Bool

newtype Subst a b = Subst { unSubst :: Map.Map a b }
    deriving Show

fromMap :: Map.Map a b -> Subst a b
fromMap = Subst

toMap :: Subst a b -> Map.Map a b
toMap = unSubst

emptySubst :: Subst a b
emptySubst = fromMap Map.empty

-- (s1 `compose` s2) `apply` t = s1 `apply` (s2 `apply` t).
compose :: (Data a, Ord a) => Subst a a -> Subst a a -> Subst a a
compose s1 s2 = fromMap (Map.unionWith const (apply s1 <$> toMap s2) (toMap s1))

apply :: Data a => Subst a b -> a -> a
apply b = transform rep
  where
    rep = undefined
    {-rep e@(Symbol s@(Pattern _)) = Map.findWithDefault e s (toMap b)-}
    {-rep x = x-}

nomatch :: MatchM a b
nomatch = return False

donematch :: MatchM a b
donematch = return True

bind :: (Eq b, Ord a) => a -> b -> MatchM a b
bind p e = do
  s0 <- get
  let s = toMap s0
  -- If the bound result is identical to previously matched
  -- value then permit the non-linear pattern.
  case Map.lookup p s of
    Just x | x == e -> return True
           | x /= e -> return False
    Nothing -> do
      put $ fromMap $ Map.insert p e s
      return True

matchesList :: a -> a -> (Bool, [(k, a)])
matchesList e p = fmap (Map.toList . toMap) $ matches e p

matches :: a -> a -> (Bool, Subst b a)
matches pat expr = runState (match pat expr) emptySubst

match :: a -> a -> MatchM b a
match x y = undefined

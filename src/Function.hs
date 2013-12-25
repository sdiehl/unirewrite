module Function where

import Match
import Data.Data

import Data.Generics.Uniplate.Data

replaceWith :: (Eq on, Data on) => on -> on -> on -> on
replaceWith x y = transform fn
  where
    fn x0 | x0 == x = y
    fn x0 = x0

-- If `expr` matches the `lhs` then rebind the `rhs` scope.
function :: Data a => b -> a -> b -> Maybe a
function lhs rhs expr =
  if matchq then
      Just $ apply subst rhs
  else
      Nothing
  where
    (matchq, subst) = matches lhs expr

matchCase :: Data a => b -> [(b, a)] -> Maybe a
matchCase _ [] = Nothing
matchCase var ((lhs, rhs):xs) =
  if matchq
  then Just $ apply subst rhs
  else matchCase var xs
  where
    (matchq, subst) = matches lhs var

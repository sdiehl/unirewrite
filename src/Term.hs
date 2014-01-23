{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Term where

import Data.Data
import qualified Data.List as List

import Data.Generics.Uniplate.Data

class (Data a, Ord a, Eq a) => IsVar a where

class (Data a, Ord a, IsVar var) => IsTerm a var | a -> var where
  isvar :: a -> Bool
  isfun :: a -> Bool

  varEx :: a -> var
  funEx :: a -> (var, [a])

  funargs :: a -> [a]
  funargs x | isfun x = snd (funEx x)
            | otherwise = error "not function"

instance IsVar Char where
instance IsVar Int where
instance IsVar Integer where
instance (Enum a, IsVar a) => IsVar [a] where

-- |Extract variable terms
variables :: IsTerm a var => a -> [var]
variables x = [varEx a | a <- universe x, isvar a]

-- |Extract function terms
functions :: IsTerm a var => a -> [a]
functions x = [a | a <- universe x, isfun a]

-- |Extract subexpressions based on a predicate.
extract :: IsTerm a var => (a -> Bool) -> a -> [a]
extract p x = [y | y <- universe x, p y]

terminal :: IsTerm a var => a -> Bool
terminal = null . children

depth :: IsTerm a var => a -> Integer
depth = para $ \_ cs -> 1 + maximum (0:cs)

-- |Extract subexpressions based on a predicate, applying a function.
extractWith :: IsTerm a var => (a -> Bool) -> (a -> b) -> a -> [b]
extractWith p f x = [f y | y <- universe x, p y]

-- |A pattern is linear if it contains each variable at most once.
isLinear :: IsTerm a var => a -> Bool
isLinear x = List.nub args == args
  where args = variables x

-- |A pattern is nonlinear if it contains a variable more than once.
isNonLinear :: IsTerm a var => a -> Bool
isNonLinear x = not (isLinear x)

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = and [x `elem` ys | x <- xs]

{-# LANGUAGE FlexibleContexts #-}

module Rule where

import Term
import Match
import Pattern

import Control.Applicative

import Data.Function
import Data.Traversable
import Data.Foldable

data Rule a = Rule
    { ruleId :: String
    , ruleFn :: a -> Maybe a
    , ruleGd :: Maybe a
    }

instance Show (Rule a) where
   show = ruleId

instance Ord (Rule a) where
   compare = compare `on` ruleId

instance Eq (Rule a) where
   (==) = (==) `on` ruleId

newtype Transform a = T { runTrans :: a -> Maybe a }

data RuleDef a = RuleDef
   { lhs :: a   -- ^ left hand side of rule
   , rhs :: a   -- ^ right hand side of rule
   } deriving (Eq)

instance Ord a => Ord (RuleDef a) where
  compare (RuleDef a b) (RuleDef c d) =
    case compare a b of
      EQ -> compare c d
      x  -> x

instance Show a => Show (RuleDef a) where
    show (RuleDef a b) = show a ++ " -> " ++ show b

instance Traversable RuleDef where
  traverse f (RuleDef l r) = RuleDef <$> f l <*> f r

instance Foldable RuleDef where
  foldMap = foldMapDefault

instance Functor RuleDef where
  fmap = fmapDefault

fromPair :: (a, a) -> RuleDef a
fromPair (x, y) = RuleDef x y

toPair :: RuleDef a -> (a, a)
toPair (RuleDef x y) = (x, y)

compileRule :: Matchable a => a -> (a -> Maybe a)
compileRule x | isRule x = pattern . ruleEx $ x
              | otherwise = const $ Just x

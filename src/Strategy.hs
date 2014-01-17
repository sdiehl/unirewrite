{-# LANGUAGE NoMonomorphismRestriction #-}

module Strategy (
  succeed,
  failure,
  seqn,

  until,
  while,

  left,
  right,

  apply,
  reduce,
  compose,
  composes,
) where

import Prelude hiding (repeat, fail, sequence, until)

import Data.Data
import Data.List
import Data.Maybe

import Control.Monad
import Control.Applicative

import Data.Generics.Uniplate.Data

-- | /Identity/
--
-- @
-- ϵ
-- @
--
-- do nothing succesfully
succeed :: a -> Maybe a
succeed = Just

-- | /Failure/
--
-- @
-- δ
-- @
--
-- Do nothing unsuccesfully
failure :: a -> Maybe a
failure = const Nothing

-- | /Sequence/
--
-- @
-- a ; b
-- @
--
-- First do a, then do that b
seqn :: MonadPlus m => (t -> m a) -> (t -> m a) -> t -> m a
seqn s t = \x -> s x `mplus` t x

-- | /Non-deterministic choice/
--
-- Left biased
--
-- @
-- a + b
-- @
--
-- Do a, if a fails then do b.
left :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
left = liftA2 (<|>)

-- | /Non-deterministic choice/
--
-- Right biased
--
-- @
-- b + a
-- @
--
-- Do b, if b fails then do a.
right :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
right = flip $ liftA2 (<|>)

-- | /Reduce/
--
-- @
-- ◯ a
-- @
--
-- Apply rule until it no longer applies.
reduce :: Data on => (on -> Maybe on) -> on -> on
reduce = rewrite

-- | /Apply/
--
-- @
-- ☐ a
-- @
--
-- Apply rule once to all subexpressions in bottom-up.

apply :: Data on => (on -> Maybe on) -> on -> on
apply f = transform g
  where g x = maybe x id (f x)

try ::  (a -> Maybe a) -> a -> Maybe a
try f = f `left` succeed

-- | Repeat while a predicate holds.

while :: Data a => (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
while p f x = if p x
  then while p f (apply f x)
  else Just x

-- | Repeat until a predicate holds.

until :: Data a => (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
until p f x = if not (p x)
  then while p f (apply f x)
  else Just x

compose :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
compose f g a = f a `mplus` g a

composes :: MonadPlus m => [t -> m a] -> t -> m a
composes fs x = msum [f x | f <- fs]

-- unbounded fixpoint
fixp :: (a -> Maybe a) -> a -> a
fixp f = last . fixpl f

fixpl :: (a -> Maybe a) -> a -> [a]
fixpl f a = a : maybe [] (fixpl f) (f a)

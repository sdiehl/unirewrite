{-# LANGUAGE NoMonomorphismRestriction #-}

module Strategy (
  succeed,
  failure,
  seqn,

  left,
  right,

  try,
  until,
  while,

  compose,
  composes,

  all,
  allM,

  topdown,
  topdownM,
  bottomup,
  bottomupM,
  apply,
  reduce,

  fixpoint,
  fixpointM
) where

import Prelude hiding (repeat, fail, sequence, until, all)

import Data.Data

import Control.Monad
import Control.Applicative

import Data.Generics.Str
import Data.Generics.Uniplate.Data

-- | /Identity/
--
-- @
-- ϵ
-- @
--
-- do nothing succesfully
succeed :: on -> Maybe on
succeed = Just

-- | /Failure/
--
-- @
-- δ
-- @
--
-- Do nothing unsuccesfully
failure :: on -> Maybe on
failure = const Nothing

-- | /Sequence/
--
-- @
-- a ; b
-- @
--
-- First do a, then do b (requiring both to succeed).
seqn :: (t -> Maybe a) -> (t -> Maybe a) -> t -> Maybe a
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
-------------------------------------------------------------------------------
-- Conditionals
-------------------------------------------------------------------------------

-- | /Try/
--
-- @
-- try f = f + id
-- @

try ::  (a -> Maybe a) -> a -> Maybe a
try f = f `left` succeed

-- | /While/
--
-- @
-- while p f
-- @
--
-- Repeat while a predicate holds.

while :: Data on => (on -> Bool) -> (on -> Maybe on) -> on -> Maybe on
while p f x = if p x
  then while p f (apply f x)
  else Just x

-- | /Until/
--
-- @
-- until p f
-- @
--
-- Repeat until a predicate holds.

until :: Data on => (on -> Bool) -> (on -> Maybe on) -> on -> Maybe on
until p f x = if not (p x)
  then while p f (apply f x)
  else Just x

-------------------------------------------------------------------------------
-- Rule Composition
-------------------------------------------------------------------------------

compose :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
compose f g a = f a `mplus` g a

composes :: [t -> Maybe a] -> t -> Maybe a
composes fs x = msum [f x | f <- fs]

-------------------------------------------------------------------------------
-- Traversal application
-------------------------------------------------------------------------------

-- | /All/
--
-- @
-- all f
-- @
--
-- applies f to all immediate child nodes

all :: Data on => (on -> on) -> on -> on
all = descend

-- | Monadic variant of bottomup.
allM :: (Monad m, Data on) => (on -> m on) -> on -> m on
allM = descendM

-- | Topdown application
--
-- @
-- topdown f = f ; all (topdown f)
-- @

topdown :: Data on => (on -> on) -> on -> on
topdown g a =
   let (current, generate) = uniplate (g a)
   in generate (strMap (topdown g) current)

-- | Monadic variant of topdown.
topdownM :: (Monad m, Data on) => (on -> m on) -> on -> m on
topdownM f = g
    where g x = f =<< topdownM g x

-- | Bottomup application
--
-- @
-- bottomup f = all (bottomup f) ; f
-- @
--
bottomup :: Data on => (on -> on) -> on -> on
bottomup = transform

-- | Monadic variant of bottomup.
bottomupM :: (Monad m, Data on) => (on -> m on) -> on -> m on
bottomupM = transformM

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

-------------------------------------------------------------------------------
-- Fixpoint
-------------------------------------------------------------------------------

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = go . iterate f
 where
   go [] = error "empty list"
   go (x:xs)
      | x == head xs = x
      | otherwise    = go xs

fixpointM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixpointM f a = do
   b <- f a
   if a == b then return a else fixpointM f b

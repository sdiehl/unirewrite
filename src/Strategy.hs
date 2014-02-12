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
import Data.Maybe

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
-- First do a, then do b (requiring both to succeed).
seqn :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
seqn s t x = s x `mplus` t x

-- | /Non-deterministic choice/
--
-- Left biased
--
-- @
-- a + b
-- @
--
-- Do a, if a fails then do b.
left :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
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
right :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
right = flip $ liftA2 (<|>)

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

while :: Data a => (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
while p f x
    | p x = while p f (apply f x)
    | otherwise = Just x

-- | /Until/
--
-- @
-- until p f
-- @
--
-- Repeat until a predicate holds.

until :: Data a => (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
until p f x
  | not (p x) = while p f (apply f x)
  | otherwise = Just x

-------------------------------------------------------------------------------
-- Rule Composition
-------------------------------------------------------------------------------

compose :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
compose f g a = f a `mplus` g a

composes :: [a -> Maybe a] -> a -> Maybe a
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

all :: Data a => (a -> a) -> a -> a
all = descend

-- | Monadic variant of bottomup.
allM :: (Monad m, Data a) => (a -> m a) -> a -> m a
allM = descendM

-- | Topdown application
--
-- @
-- topdown f = f ; all (topdown f)
-- @

topdown :: Data a => (a -> a) -> a -> a
topdown g a =
   let (current, generate) = uniplate (g a)
   in generate (strMap (topdown g) current)

-- | Monadic variant of topdown.
topdownM :: (Monad m, Data a) => (a -> m a) -> a -> m a
topdownM f = g
    where g x = f =<< topdownM g x

-- | Bottomup application
--
-- @
-- bottomup f = all (bottomup f) ; f
-- @
--
bottomup :: Data a => (a -> a) -> a -> a
bottomup = transform

-- | Monadic variant of bottomup.
bottomupM :: (Monad m, Data a) => (a -> m a) -> a -> m a
bottomupM = transformM

-- | /Reduce/
--
-- @
-- ◯ a
-- @
--
-- Apply rule until it no longer applies.

reduce :: Data a => (a -> Maybe a) -> a -> a
reduce = rewrite

-- | /Apply/
--
-- @
-- ☐ a
-- @
--
-- Apply rule once to all subexpressions in bottom-up.

apply :: Data a => (a -> Maybe a) -> a -> a
apply f = transform g
  where g x = fromMaybe x (f x)

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

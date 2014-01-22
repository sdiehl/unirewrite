module Index (
  labeled,
  child,

  applyTo,
  applyToM,

  applyAt,
  applyAtM,

  somewhere,
  somewhereM,
) where

import Data.Data
import Control.Monad

import Data.Generics.Str
import Data.Generics.Uniplate.Data

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

indexed :: (Enum i, Num i, Data on) => on -> [(i, on)]
indexed tree = zip [0..] $ children tree

-- | Label each expression with it's path.
labeled :: (Enum i, Num i, Data on) => on -> [([i], on)]
labeled a = ([], a) : [ (x:xs, b) | (x, c)  <- indexed a,
                                    (xs, b) <- labeled c ]

-- |Extract the nth child of the top expression
child :: Data a => Int -> a -> Maybe a
child n = safeHead . drop n . children

-------------------------------------------------------------------------------
-- Suberm application
-------------------------------------------------------------------------------

{-# INLINE uniplateList #-}
uniplateList :: Data c => c -> ([c], [c] -> c)
uniplateList x = (c, b . d)
    where
        (a,b) = uniplate x
        (c,d) = strStructure a

-- | Apply a function to one immediate child.
applyTo :: (Data on ) => Int -> (on -> on) -> on -> on
applyTo n f a =
   let (current, generate) = uniplateList a
       g i = if i==n then f else id
   in generate (zipWith g [0..] current)

-- | Monadic version of applyTo
applyToM :: (Monad m, Data a) => Int -> (a -> m a) -> a -> m a
applyToM n f a =
   let (current, generate) = uniplateList a
       g (i, b) = if i==n then f b else return b
   in liftM generate $ mapM g (zip [0..] current)

-- | Apply a function at a given path
applyAt :: Data a => [Int] -> (a -> a) -> a -> a
applyAt is f = foldr applyTo f is

-- | Monadic variant of applyAt
applyAtM :: (Monad m, Data a) => [Int] -> (a -> m a) -> a -> m a
applyAtM is f = foldr applyToM f is

somewhere :: Data a => (a -> a) -> a -> [a]
somewhere f = somewhereM (return . f)

somewhereM :: (MonadPlus m, Data a) => (a -> m a) -> a -> m a
somewhereM f a = msum $ f a : map g [0..n-1]
 where
   n   = length (children a)
   g i = applyToM i (somewhereM f) a

safeHead :: [a] -> Maybe a
safeHead (x:_) = return x
safeHead _     = Nothing

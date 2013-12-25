module Strategy (
) where

import Data.Data

import Control.Monad
import Control.Applicative

import Data.Generics.Uniplate.Data

succeed :: a -> Maybe a
succeed = Just

fail :: a -> Maybe a
fail = const Nothing

-- r1 > r2
-- Apply r1, if it succeeds then apply r2
composeM :: Monad m => [a -> m a] -> a -> m a
composeM = foldr (>=>) return

left :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
left = liftA2 (<|>)

right :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
right = flip $ liftA2 (<|>)

lefts :: Data a => [a -> Maybe a] -> a -> Maybe a
lefts = foldl1 left

try ::  (a -> Maybe a) -> a -> Maybe a
try f = f `left` succeed

rights :: [a -> Maybe a] -> a -> Maybe a
rights = foldl1 right

-- Repeatedly apply (r1 | r2 | ... rn) until rn fails.
switch ::  Data on => [on -> Maybe on] -> on -> on
switch = rewrite . lefts

series :: Data a => [a -> Maybe a] -> a -> Maybe a
series = lefts . map ((Just .) . rewrite)

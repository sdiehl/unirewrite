{-# LANGUAGE FlexibleContexts #-}

module Unirewrite (
) where

import Data.Data

import Control.Monad
import Control.Monad.State

import Data.Generics.Uniplate.Data

-------------------------------------------------------------------------------
-- Composite Uniplate
-------------------------------------------------------------------------------

composeM :: MonadPlus m => [t -> m a] -> t -> m a
composeM ts x = msum [t x | t <- ts]

{-composeM :: Monad m => [a -> m a] -> a -> m a-}
{-composeM = foldr (>=>) return-}

rewrites :: Data on => [on -> Maybe on] -> on -> on
rewrites = rewrite . composeM

{-rewritesM = rewriteM . composeM-}

-------------------------------------------------------------------------------
-- Rewrite Termination
-------------------------------------------------------------------------------

data Status = Success | Failed deriving (Show)
{-data Status a = Success a | Failed deriving (Show)-}

istate :: (Status, Integer)
istate = (Success, 0)

-- |Perform a rewrite a maximum of `u` times before failing. Uses
-- State to track success of failure of rewrite.
limit :: Show b => Integer -> (b -> Maybe b) -> b -> State (Status, Integer) (Maybe b)
limit u f x = do
  (_, i) <- get
  if i < u then do
    put (Success, i + 1)
    let res = f x
    return res
    {-return $ trace ("Iter" ++ (show res)) res-}
  else do
    put (Failed, i + 1)
    return Nothing

-- Transform a rule to be recursion limited, fails to apply if
-- not terminated.
limitrule ::  Show b => Integer
                     -> (b -> Maybe b)
                     -> (b -> Maybe b)
limitrule u f x = evalState (limit u f x) istate

abort :: MonadState (Status, t) m => m ()
abort = modify $ \s -> (Failed, snd s)

rewriten :: (Data a, Show a) => Integer
                             -> (a -> Maybe a)
                             -> a -> (a, (Status, Integer))
rewriten n f x = runState (rewriteM (limit n f) x) istate

rewritesn :: (Data a, Show a) => Integer
                              -> [a -> Maybe a]
                              -> a -> (a, (Status, Integer))
rewritesn n f x = runState (rewriteM (limit n (composeM f)) x) istate

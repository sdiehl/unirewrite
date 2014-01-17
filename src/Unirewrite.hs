{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unirewrite (
  evaluatorLoop,
  EvalState,
  Trans
) where

import Control.Monad.RWS
import Control.Monad.Reader as R

import Data.Data
import Data.Generics.Uniplate.Data

-------------------------------------------------------------------------------
-- Evaluation State
-------------------------------------------------------------------------------

data Status = Success | Failed deriving (Show)

data EvalState = EvalState
    { depth :: Integer
    , iter  :: Integer
    , status :: Status
    , maxRecursion :: Integer
    , maxIteration :: Integer
    } deriving (Show)

defaultEvalState :: EvalState
defaultEvalState = EvalState 0 0 Success 50 50

incDepth :: Eval c a ()
incDepth = modify $ \s -> s { depth = (depth s) + 1 }

decDepth :: Eval c a ()
decDepth = modify $ \s -> s { depth = (depth s) - 1 }

incIter :: Eval c a ()
incIter = modify $ \s -> s { iter = (iter s) + 1 }

decIter :: Eval c a ()
decIter = modify $ \s -> s { iter = (iter s) - 1 }

-------------------------------------------------------------------------------
-- Eval Loop
-------------------------------------------------------------------------------

type Trans t a = a -> ReaderT t IO (String, Maybe a)

type Eval c a r = RWST c        -- ^Evaluation context (properties, definitions)
                  [(String, a)] -- ^Steps
                  EvalState     -- ^Evaluation state
                  IO
                  r             -- ^Result

limitReached :: Eval c a Bool
limitReached = do
  i <- gets depth
  j <- gets iter
  maxi <- gets maxRecursion
  maxj <- gets maxIteration
  return $ i > maxi || j > maxj

runTrans :: c -> (a -> ReaderT c IO (String, (Maybe a))) -> a -> IO (String, Maybe a)
runTrans ctx f x = runReaderT (f x) ctx

addStep :: String -> a -> Eval c a ()
addStep rl s = do
  i <- gets depth
  if i == 0 then do
    tell [(rl, s)]
  else do
    return ()

eval :: (Eq a, Data a, Show a) => (Trans c a) -> a -> Eval c a a
eval f x = do
  ctx <- ask
  abort <- limitReached
  if abort then do
    modify $ \s -> s { status = Failed }
    return x
  else do
    -- transform children in bottom-up applicative order
    incDepth
    y <- descendM (eval f) x
    tell [("",y)]
    decDepth
    (rl, z) <- lift $ runTrans ctx f y
    -- apply to the root
    case z of
      Just r -> do
        addStep rl x
        incIter
        eval f r
      -- If in normal form then halt
      Nothing -> do
        return y

evaluatorLoop :: (Eq a, Data a, Show a) => c -> Trans c a -> a -> IO (a, EvalState, [(String, a)])
evaluatorLoop ctx f x = runRWST (eval f x) ctx defaultEvalState

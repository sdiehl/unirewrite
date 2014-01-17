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
    } deriving (Show)

defaultEvalState :: EvalState
defaultEvalState = EvalState 0 0 Success

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

type Trans t a = a -> Reader t (String, Maybe a)

type Eval c a r = RWS c         -- ^Evaluation context (properties, definitions)
                  [(String, a)] -- ^Steps
                  EvalState     -- ^Evaluation state
                  r             -- ^Result

limitReached :: Eval c a Bool
limitReached = do
  i <- gets depth
  j <- gets iter
  return $ i > 50 || j > 50

runTrans :: c -> (a -> R.Reader c (String, (Maybe a))) -> a -> (String, Maybe a)
runTrans ctx f x = R.runReader (f x) ctx

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
    decDepth
    -- apply to the root
    let (rl, z) = runTrans ctx f y
    case z of
      Just r -> do
        addStep rl x
        incIter
        eval f r
      -- If in normal form then halt
      Nothing -> do
        return y

evaluatorLoop :: (Eq a, Data a, Show a) => c -> Trans c a -> a -> (a, EvalState, [(String, a)])
evaluatorLoop ctx f x = runRWS (eval f x) ctx defaultEvalState

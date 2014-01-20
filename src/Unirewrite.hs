{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unirewrite (
  evaluatorLoop,
  Step,
  EvalState,
  Trans,
  Dir,

  Direction(..),
  Evalutable
) where

import Control.Monad.RWS
import Control.Monad.Reader as R

import Data.Data
import Data.Generics.Uniplate.Data

-------------------------------------------------------------------------------
-- Evaluation Classes
-------------------------------------------------------------------------------

class (Eq a, Data a, Show a) => Evalutable a

-------------------------------------------------------------------------------
-- Evaluation State
-------------------------------------------------------------------------------

data Status = Success | Failed deriving (Show)

data EvalState = EvalState
    { depth :: Integer
    , iter  :: Integer
    , status :: Status
    , aborted :: Bool
    , maxRecursion :: Integer
    , maxIteration :: Integer
    } deriving (Show)

defaultEvalState :: EvalState
defaultEvalState = EvalState
                  { depth = 0
                  , iter = 0
                  , status = Success
                  , aborted = False
                  , maxRecursion = 4096
                  , maxIteration = 4096
                  }

incDepth :: Eval c a ()
incDepth = modify $ \s -> s { depth = (depth s) + 1 }

decDepth :: Eval c a ()
decDepth = modify $ \s -> s { depth = (depth s) - 1 }

incIter :: Eval c a ()
incIter = modify $ \s -> s { iter = (iter s) + 1 }

abort :: Eval c a ()
abort = modify $ \s -> s { aborted = True }

-------------------------------------------------------------------------------
-- Eval Loop
-------------------------------------------------------------------------------

-- Term transformations
type Trans t a = a -> ReaderT t IO (Step a)

-- Evaluation directives
type Dir t a = a -> ReaderT t IO Direction
data Direction = Pass | BottomUp | TopDown | Abort | Some [Bool]

type Step a = (String, Maybe a)

type Eval c a r = RWST c        -- ^Evaluation context (properties, definitions)
                  [Step a]      -- ^Steps
                  EvalState     -- ^Evaluation state
                  IO            -- ^Underlying IO
                  r             -- ^Result

eval :: (Evalutable a) => Dir c a -> Trans c a -> a -> Eval c a a
eval d f x  = do
  ctx <- ask
  stop <- limitReached
  if stop then do
    modify $ \s -> s { status = Failed }
    return x

  else do
    director <- lift $ runDir ctx d x
    case director of

      -- transform children in bottom-up applicative order
      BottomUp -> do
        incDepth
        y <- descendM (eval d f) x
        decDepth
        step@(_, z) <- lift $ runTrans ctx f y
        -- apply to the root
        case z of
          Just r -> do
            addStep step
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> do
            return y

      TopDown -> do
        step@(_, y) <- lift $ runTrans ctx f x
        -- apply to the root
        z <- case y of
          Just r -> do
            addStep step
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> do
            return x
        incDepth
        res <- descendM (eval d f) z
        decDepth
        return res

      -- do not proceed to children
      Pass -> do
        step@(_, z) <- lift $ runTrans ctx f x
        case z of
          Just r -> do
            addStep step
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> do
            return x

      Abort -> do
        abort
        return x


limitReached :: Eval c a Bool
limitReached = do
  i <- gets depth
  j <- gets iter
  stop <- gets aborted
  maxi <- gets maxRecursion
  maxj <- gets maxIteration
  return $ stop || i > maxi || j > maxj

runTrans :: c -> Trans c a -> a -> IO (String, Maybe a)
runTrans ctx f x = runReaderT (f x) ctx

runDir :: c -> Dir c a -> a -> IO Direction
runDir ctx d x = runReaderT (d x) ctx

addStep :: Step a -> Eval c a ()
addStep step = do
  i <- gets depth
  if i == 0 then do
    tell [step]
  else do
    return ()

evaluatorLoop :: (Evalutable a) => c -> Dir c a -> Trans c a -> a -> IO (a, EvalState, [Step a])
evaluatorLoop ctx d f x = runRWST (eval d f x) ctx defaultEvalState

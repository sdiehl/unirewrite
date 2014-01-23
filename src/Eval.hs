{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Eval (
  -- * Evaluator
  Eval,
  runEval,
  evalRec,

  -- * Evalaution state
  Dir,
  Step,
  Status(..),
  EvalState(..),
  Derivation(..),
  Trans,

  -- * Classes
  Evalutable(..),
  Direction(..)
) where

import Control.Applicative
import Control.Monad.RWS
import Control.Monad.Reader as R

import Data.Data
import Data.Generics.Uniplate.Data

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

type Witness = String
type Trans c a = (a -> Eval c a (Witness, Maybe a))

-------------------------------------------------------------------------------
-- Derivations
-------------------------------------------------------------------------------

-- Derivation
data Step a = Step Witness a (Maybe a)
  deriving (Eq)

newtype Derivation a = Derivation { unDerivation :: [Step a] }
  deriving (Eq, Monoid)

instance Show a => Show (Step a) where
  show (Step rl x (Just y)) = rl ++ " : " ++ show x ++ " -> " ++ show y ++ "."
  show (Step rl x Nothing) = rl ++ " : " ++ show x  ++ "."

instance Show a => Show (Derivation a) where
  show (Derivation xs) = unlines (map show xs)

-------------------------------------------------------------------------------
-- Evaluation Classes
-------------------------------------------------------------------------------

class (Eq a, Data a, Show a) => Evalutable a where

-------------------------------------------------------------------------------
-- Evaluation State
-------------------------------------------------------------------------------

data Status = Success | Failed deriving (Eq, Ord, Show)

data EvalState c a = EvalState
    { depth        :: Integer
    , iter         :: Integer
    , status       :: Status
    , aborted      :: Bool
    , maxRecursion :: Integer
    , maxIteration :: Integer
    , self         :: a -> Eval c a a
    }

defaultEvalState :: EvalState c a
defaultEvalState = EvalState
    { depth        = 0
    , iter         = 0
    , status       = Success
    , aborted      = False
    , maxRecursion = 4096
    , maxIteration = 4096
    , self         = undefined
    }

incDepth :: Eval c a ()
incDepth = modify $ \s -> s { depth = depth s + 1 }

decDepth :: Eval c a ()
decDepth = modify $ \s -> s { depth = depth s - 1 }

incIter :: Eval c a ()
incIter = modify $ \s -> s { iter = iter s + 1 }

abort :: Eval c a ()
abort = modify $ \s -> s { aborted = True }

failed :: Eval c a ()
failed = modify $ \s -> s { status = Failed }

-------------------------------------------------------------------------------
-- Eval Loop
-------------------------------------------------------------------------------

-- Evaluation directives
data Direction = BottomUp | TopDown | Pass | Abort | Some [Bool]
type Dir c a = a -> ReaderT c IO Direction

type Eval c a r = (RWST
                     c               --  Evaluation context
                     (Derivation a)  --  Steps
                     (EvalState c a) --  Evaluation state
                     IO              --  Underlying IO
                     r)              --  Result

eval :: Evalutable a => Dir c a -> (a -> Eval c a (Witness, Maybe a)) -> a -> Eval c a a
eval d f x  = do
  ctx <- ask
  stop <- limitReached
  if stop then do
    failed
    return x
  else do
    dir <- lift $ runDir d x ctx
    case dir of

      -- transform children in bottom-up applicative order
      BottomUp -> do
        incDepth
        y <- descendM (eval d f) x
        decDepth
        step@(_, z) <- f x
        -- apply to the root
        case z of
          Just r -> do
            addStep (mkStep y step)
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> return y

      TopDown -> do
        step@(_, y) <- f x
        -- apply to the root
        z <- case y of
          Just r -> do
            addStep (mkStep x step)
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> return x
        incDepth
        res <- descendM (eval d f) z
        decDepth
        return res

      -- do not proceed to children
      Pass -> do
        step@(_, z) <- f x
        case z of
          Just r -> do
            addStep (mkStep x step)
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> return x

      Some xs -> undefined

      Abort -> do
        abort
        return x

mkStep :: a -> (String, Maybe a) -> Step a
mkStep orig (rl, res) = Step rl orig res

limitReached :: Eval c a Bool
limitReached = do
  i <- gets depth
  j <- gets iter
  stop <- gets aborted
  maxi <- gets maxRecursion
  maxj <- gets maxIteration
  return $ stop || i > maxi || j > maxj

runDir :: Dir c a -> a -> c -> IO Direction
runDir d x = runReaderT (d x)

addStep :: Step a -> Eval c a ()
addStep step = do
  i <- gets depth
  when (i == 0) $ tell (Derivation [step])

runEval :: (Evalutable a) => c -> Dir c a -> Trans c a -> a -> IO (a, EvalState c a, Derivation a)
runEval ctx d f x = runRWST (eval d f x) ctx (defaultEvalState { self = eval d f })

-- | Embed a sub-evaluator within another evaluator, sharing the same initial state but yielding a pure result
-- to the external one and the extending the external state with the resulting artifacts.
--
-- @
--   Eval s a (Eval t a) --> Eval (join s t) a
--   +-------------+         +-------------+
--   |             |         |             |
--   |  +-----+    |         |             |
--   |  |  a  | ---|---------|--->  a      |
--   |  +-----+    |         |             |
--   |             |         |             |
--   +-------------+         +-------------+
-- @
--
evalRec :: (Evalutable a) => a -> Eval c a (Maybe a)
evalRec x = do
  st <- get
  ctx <- ask
  (res, st', deriv) <- lift $ runRWST (self st $ x) ctx st
  if status st == Success then do
    tell deriv
    put st'
    return $ Just res
  else
    return Nothing

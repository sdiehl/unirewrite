{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Eval (
  -- * Evaluator
  runEval,

  -- * Evalaution state
  Dir,
  Step,
  Status(..),
  EvalState(..),
  Derivation(..),

  -- * Transformer
  TransformerState(..),
  TransM,
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

-- Term transformations
type TransM c a = a -> ReaderT (TransformerState c a) IO (Witness, Maybe a)
type Trans c a = a -> Eval c a a

data TransformerState c a = TransformerState
    { teval :: a -> Eval c a a
    , env  :: c
    }

runTrans :: TransM c a -> a -> Trans c a -> c -> IO (Witness, Maybe a)
runTrans f x ev ctx = runReaderT (f x) (TransformerState ev ctx)

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
    }

defaultEvalState :: EvalState c a
defaultEvalState = EvalState
    { depth        = 0
    , iter         = 0
    , status       = Success
    , aborted      = False
    , maxRecursion = 4096
    , maxIteration = 4096
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

type Witness = String

-- Evaluation directives
data Direction = BottomUp | TopDown | Pass | Abort | Some [Bool]
type Dir c a = a -> ReaderT c IO Direction

-- Derivation
data Step a = Step Witness a (Maybe a)
  deriving (Eq)

newtype Derivation a = Derivation { unDerivation :: [Step a] }
  deriving (Eq, Monoid)

type Eval c a r = (RWST
                     c               --  Evaluation context
                     (Derivation a)  --  Steps
                     (EvalState c a) --  Evaluation state
                     IO              --  Underlying IO
                     r)              --  Result

eval :: Evalutable a => Dir c a -> TransM c a -> a -> Eval c a a
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
        step@(_, z) <- lift $ runTrans f x (eval d f) ctx
        -- apply to the root
        case z of
          Just r -> do
            addStep (mkStep y step)
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> return y

      TopDown -> do
        step@(_, y) <- lift $ runTrans f x (eval d f) ctx
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
        step@(_, z) <- lift $ runTrans f x (eval d f) ctx
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

runEval :: (Evalutable a) => c -> Dir c a -> TransM c a -> a -> IO (a, EvalState c a, Derivation a)
runEval ctx d f x = runRWST (eval d f x) ctx defaultEvalState

instance Show a => Show (Step a) where
  show (Step rl x (Just y)) = rl ++ " : " ++ show x ++ " -> " ++ show y ++ "."
  show (Step rl x Nothing) = rl ++ " : " ++ show x  ++ "."

instance Show a => Show (Derivation a) where
  show (Derivation xs) = unlines (map show xs)

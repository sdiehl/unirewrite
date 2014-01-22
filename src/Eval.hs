{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Eval (
  -- * Evaluator
  evaluatorLoop,

  -- * Evalaution state
  Dir,
  Step,
  Trans,
  EvalState,
  Derivation,

  -- * Classes
  Evalutable,
  Direction(..)
) where

import Control.Monad.RWS
import Control.Monad.Reader as R

import Data.Data
import Data.Monoid
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
    { depth        = 0
    , iter         = 0
    , status       = Success
    , aborted      = False
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

failed :: Eval c a ()
failed = modify $ \s -> s { status = Failed }

-------------------------------------------------------------------------------
-- Eval Loop
-------------------------------------------------------------------------------

-- Term transformations
type Trans t a = a -> ReaderT t IO (String, Maybe a)

-- Evaluation directives
data Direction = BottomUp | TopDown | Pass | Abort | Some [Bool]
type Dir t a = a -> ReaderT t IO Direction

-- Derivation
data Step a = Step String a (Maybe a)

newtype Derivation a = Derivation { unDerivation :: [Step a] }
  deriving Monoid

type Eval c a r = (RWST
                     c              --  Evaluation context
                     (Derivation a) --  Steps
                     EvalState      --  Evaluation state
                     IO             --  Underlying IO
                     r)             --  Result

eval :: (Evalutable a) => Dir c a -> Trans c a -> a -> Eval c a a
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
        step@(_, z) <- lift $ runTrans f y ctx
        -- apply to the root
        case z of
          Just r -> do
            addStep (mkStep y step)
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> do
            return y

      TopDown -> do
        step@(_, y) <- lift $ runTrans f x ctx
        -- apply to the root
        z <- case y of
          Just r -> do
            addStep (mkStep x step)
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
        step@(_, z) <- lift $ runTrans f x ctx
        case z of
          Just r -> do
            addStep (mkStep x step)
            incIter
            eval d f r
          -- If in normal form then halt
          Nothing -> do
            return x

      Some xs -> do
        undefined

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

runTrans :: Trans c a -> a -> c -> IO (String, Maybe a)
runTrans f x = runReaderT (f x)

runDir :: Dir c a -> a -> c -> IO Direction
runDir d x = runReaderT (d x)

addStep :: Step a -> Eval c a ()
addStep step = do
  i <- gets depth
  if i == 0 then
    tell (Derivation [step])
  else do
    tell (Derivation [step])
    {-return ()-}

evaluatorLoop :: (Evalutable a) => c -> Dir c a -> Trans c a -> a -> IO (a, EvalState, Derivation a)
evaluatorLoop ctx d f x = runRWST (eval d f x) ctx defaultEvalState


instance Show a => Show (Step a) where
  show (Step rl x (Just y)) = rl ++ " : " ++ show x ++ " -> " ++ show y ++ "."
  show (Step rl x Nothing) = rl ++ " : " ++ show x  ++ "."

instance Show a => Show (Derivation a) where
  show (Derivation xs) = unlines (map show (xs))

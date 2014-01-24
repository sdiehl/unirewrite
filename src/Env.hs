{-# LANGUAGE MultiParamTypeClasses #-}

module Env where

import Data.Hashable

class (Eq a, Ord a, Hashable a) => Identifiable a b where
  identify :: a -> b

class Identifiable a b => Environment a b where

{-# LANGUAGE MultiParamTypeClasses #-}

module Env where

class Identifiable a b where
  identify :: a -> b

class Identifiable a b => Environment a b where

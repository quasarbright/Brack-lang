{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Module where

import Brack.Syntax.Statement

data Module a = Module [Statement a] a deriving(Eq, Ord, Show, Functor)
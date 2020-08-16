{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Module where

import Brack.Syntax.Statement

data Module a = Module [Statement a] a deriving(Eq, Ord, Functor)

instance Show (Module a) where show (Module stmts _) = unwords (show <$> stmts)
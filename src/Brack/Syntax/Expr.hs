{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Expr where

import Brack.Syntax.Name
import Brack.Syntax.Type

data Literal a = LInt Integer a
               | LDouble Double a
               | LChar Char a
               | LBool Bool a
               deriving(Eq, Ord, Show, Functor)

data Expr a = Var (QName a) a
            | Lit (Literal a) a
            deriving(Eq, Ord, Show, Functor)

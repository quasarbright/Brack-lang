{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Expr where

import Brack.Utils.Common
import Brack.Syntax.Name

data Literal a = LInt Integer a
               | LDouble Double a
               | LChar Char a
               | LBool Bool a
               deriving(Eq, Ord, Functor)

data Expr a = Var (QName a) a
            | Lit (Literal a) a
            | Paren (Expr a) a
            deriving(Eq, Ord, Functor)

instance Show (Literal a) where
    show l_ = case l_ of
        LInt n _ -> show n
        LDouble d _ -> show d
        LChar c _ -> show c
        LBool True _ -> "true"
        LBool False _ -> "false"

instance Show (Expr a) where
    show e_ = case e_ of
        Var name _ -> show name
        Lit l _ -> show l
        Paren e _ -> concat["(",show e,")"]

instance Tagged Expr where
    getTag e_ = case e_ of
        Var _ a -> a
        Lit _ a -> a
        Paren _ a -> a
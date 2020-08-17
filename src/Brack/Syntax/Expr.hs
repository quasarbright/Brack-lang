{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Expr where

import Brack.Utils.Common
import Brack.Syntax.Name

data Literal a = LInt Integer a
               | LDouble Double a
               | LChar Char a
               | LBool Bool a
               deriving(Eq, Ord, Functor)

data Prim1 = Negate | Not deriving(Eq, Ord)

data Prim2 = Plus | Minus | Times | Divide | Modulo | Pow | Less | Greater | LessEq | GreaterEq | Equals | NotEquals | Or | And deriving (Eq, Ord)

data Expr a = Var (QName a) a
            | Lit (Literal a) a
            | Prim1 Prim1 (Expr a) a
            | Prim2 (Expr a) Prim2 (Expr a) a
            | Paren (Expr a) a
            deriving(Eq, Ord, Functor)

instance Show (Literal a) where
    show l_ = case l_ of
        LInt n _ -> show n
        LDouble d _ -> show d
        LChar c _ -> show c
        LBool True _ -> "true"
        LBool False _ -> "false"

instance Show Prim1 where
    show Not = "!"
    show Negate = "-"

instance Show Prim2 where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"
    show Divide = "/"
    show Modulo = "%"
    show Pow = "^"
    show Less = "<"
    show Greater = ">"
    show LessEq = "<="
    show GreaterEq = ">="
    show Equals = "=="
    show NotEquals = "!="
    show Or = "||"
    show And = "&&"

instance Show (Expr a) where
    show e_ = case e_ of
        Var name _ -> show name
        Lit l _ -> show l
        Prim1 prim1 e _ -> show prim1 ++ show e
        Prim2 left prim2 right _ -> unwords [show left, show prim2, show right]
        Paren e _ -> concat["(",show e,")"]

instance Tagged Expr where
    getTag e_ = case e_ of
        Var _ a -> a
        Lit _ a -> a
        Prim1 _ _ a -> a
        Prim2 _ _ _ a -> a
        Paren _ a -> a
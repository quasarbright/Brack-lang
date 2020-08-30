{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Statement where

import Brack.Utils.Common
import Brack.Syntax.Name
import Brack.Syntax.Type
import Data.List (intercalate)

{-
TODO for loops
TODO printing
TODO lists
TODO rec
-}

data Statement a = Definition (QName a) (Type a) (Expr a) a
                 | FunctionDefinition (QName a) [(QName a, Type a)] (Type a) [Statement a] a
                 | Assignment (QName a) (Expr a) a
                 | Execution (Expr a) a
                 | If (Expr a) [Statement a] (Maybe [Statement a]) a
                 | While (Expr a) [Statement a] a
                 | Return (Expr a) a
                 | Break a
                 | Continue a
                 deriving(Eq, Ord, Functor)

instance Show (Statement a) where
    show s_ = case s_ of
        Definition name t e _ -> concat [show name, " :: ", show t, " = ", show e, ";"]
        FunctionDefinition name typedArgs retType body _ ->
            "def "++show name++"("++intercalate ", " typedArgsStrs++") -> "++show retType++" "++show body
                where typedArgsStrs = [show argName++" :: "++show argType | (argName, argType) <- typedArgs]
        Assignment name e _ -> concat [show name, " = ", show e, ";"]
        Execution e _ -> show e ++ ";"
        If cnd thn m_els _ -> concat ["if (", show cnd, ") ", show thn, els']
            where
                els' = case m_els of
                    Nothing -> ""
                    Just els -> " else " ++ show els
        While cnd body _ -> "while ("++show cnd++") " ++ show body
        Return e _ -> "return " ++ show e ++ ";"
        Break _ -> "break;"
        Continue _ -> "continue;"
    showList statements = showString "{ " . showString (unwords (show <$> statements)) . showString " }"

instance Tagged Statement where
    getTag s_ = case s_ of
        Definition _ _ _ a -> a
        FunctionDefinition _ _ _ _ a -> a
        Assignment _ _ a -> a
        Execution _ a -> a
        If _ _ _ a -> a
        While _ _ a -> a
        Return _ a -> a
        Break a -> a
        Continue a -> a

-- exprs

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
            | Application (Expr a) [Expr a] a
            | Function [(QName a, Type a)] (Type a) [Statement a] a
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
        Application f args _ -> show f ++ "(" ++ intercalate ", " (show <$> args) ++ ")"
        Function typedArgs retType body _ -> "function(" ++ intercalate ", " typedArgsStrs ++ ") -> "++show retType++" "++ show body
            where typedArgsStrs = [show name++" :: "++show argType | (name, argType) <- typedArgs]
        Paren e _ -> concat["(",show e,")"]

instance Tagged Expr where
    getTag e_ = case e_ of
        Var _ a -> a
        Lit _ a -> a
        Prim1 _ _ a -> a
        Prim2 _ _ _ a -> a
        Application _ _ a -> a
        Function _ _ _ a -> a
        Paren _ a -> a
{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Statement where

import Brack.Utils.Common
import Brack.Syntax.Name
import Brack.Syntax.Type
import Brack.Syntax.Expr

data Statement a = Definition (QName a) (Type a) (Expr a) a
                 | Assignment (QName a) (Expr a) a
                 | Execution (Expr a) a
                 | If (Expr a) [Statement a] (Maybe [Statement a]) a
                 | While (Expr a) [Statement a] a
                 deriving(Eq, Ord, Functor)

instance Show (Statement a) where
    show s_ = case s_ of
        Definition name t e _ -> concat [show name, " :: ", show t, " = ", show e, ";"]
        Assignment name e _ -> concat [show name, " = ", show e, ";"]
        Execution e _ -> show e ++ ";"
        If cnd thn m_els _ -> concat ["if (", show cnd, ") { ", thn', " }", els']
            where
                thn' = unwords (show <$> thn)
                els' = case m_els of
                    Nothing -> ""
                    Just els -> " else { "++unwords (show <$> els) ++ " }"
        While cnd body _ -> "while ("++show cnd++") { "++unwords (show <$> body)++" }"

instance Tagged Statement where
    getTag s_ = case s_ of
        Definition _ _ _ a -> a
        Assignment _ _ a -> a
        Execution _ a -> a
        If _ _ _ a -> a
        While _ _ a -> a
{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Type where

import Brack.Syntax.Name
import Data.List (intercalate)

data Type a = TInt a
            | TDouble a
            | TBool a
            | TChar a
            | TVoid a
            | TCon (QName a) a
            | TArr [Type a] (Type a) a
            deriving(Eq, Ord, Functor)

instance Show (Type a) where
    show t_ = case t_ of
        TInt{} -> "int"
        TDouble{} -> "double"
        TBool{} -> "bool"
        TChar{} -> "char"
        TVoid{} -> "void"
        TCon name _ -> show name
        TArr args ret _ -> "("++intercalate ", " (show <$> args) ++ ") -> " ++ show ret
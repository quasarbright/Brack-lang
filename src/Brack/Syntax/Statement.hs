{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Statement where

import Brack.Syntax.Name
import Brack.Syntax.Type
import Brack.Syntax.Expr

data Statement a = Definition (Name a) (Type a) (Expr a) a
                 | Assignment (Name a) (Expr a) a
                 | Execution (Expr a) a
                 | If (Expr a) [Statement a] (Maybe [Statement a]) a
                 deriving(Eq, Ord, Show, Functor)

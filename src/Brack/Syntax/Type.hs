{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Type where

import Brack.Syntax.Name

data Type a = TInt a
            | TDouble a
            | TBool a
            | TChar a
            | TVoid a
            | TCon (QName a) a
            deriving(Eq, Ord, Show, Functor)
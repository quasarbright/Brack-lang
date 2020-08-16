{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Name where

data ModuleName a = ModuleName String a deriving(Eq, Ord, Show, Functor)

data Name a = LowerIdent String a
            | UpperIdent String a
            deriving(Eq, Ord, Show, Functor)

data QName a = Qualified (ModuleName a) (Name a)
             | UnQualified (Name a)
             deriving(Eq, Ord, Show, Functor)
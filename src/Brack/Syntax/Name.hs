{-# LANGUAGE DeriveFunctor #-}
module Brack.Syntax.Name where

data ModuleName a = ModuleName String a deriving(Eq, Ord, Functor)

data Name a = LowerIdent String a
            | UpperIdent String a
            deriving(Eq, Ord, Functor)

data QName a = Qualified (ModuleName a) (Name a) a
             | UnQualified (Name a) a
             deriving(Eq, Ord, Functor)

instance Show (ModuleName a) where
    show (ModuleName name _) = name
             
instance Show (Name a) where
    show n_ = case n_ of
        LowerIdent name _ -> name
        UpperIdent name _ -> name

instance Show (QName a) where
    show q_ = case q_ of
        Qualified mname name _ -> concat[show mname,".",show name]
        UnQualified name _ -> show name
module Brack.Dynamic.DynamicError where

data DynamicError a = InternalError String a
                    | DivideByZero a deriving (Eq, Ord, Show)


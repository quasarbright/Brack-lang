module Brack.Dynamic.DynamicError where

data DynamicError a = InternalError String a deriving (Eq, Ord, Show)


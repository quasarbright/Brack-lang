module Brack.Static.StaticError where

import Brack.Syntax.Type
import Brack.Syntax.Name

data StaticError a = TypeMismatch (Type a) (Type a) a
                   | UnboundVar (QName a) a
                   deriving(Eq, Ord, Show)
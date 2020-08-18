module Brack.Static.WellFormed where

{-
ensure functions have return statements in every branch unless void
ensure no unbound variables
nothing takes in void
if you end up allowing declarations without instantiation (like x :: int;), ensure variables are definitely
    initialized before usage
functions can't have duplicate argument names
no return in void (currently don't allow return;)

warning for unused variables
warning for same-scope shadowing
break only in loops
return only in functions
only fun defs in rec
shadowing in rec groups (and f(f, x, ...) self-shadowing)
-}
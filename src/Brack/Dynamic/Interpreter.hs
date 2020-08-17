module Brack.Dynamic.Interpreter where

import Brack.Utils.Common

import Brack.Dynamic.Memory
import Brack.Dynamic.DynamicError

--import Brack.Syntax.Name
import Brack.Syntax.Expr
import Brack.Syntax.Statement
import Brack.Syntax.Module

import Control.Monad
--import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

type InterpreterState a = Memory a

type Interpreter a b = StateExceptT (InterpreterState a) (DynamicError a) b

runModule :: Eq a => Module a -> Interpreter a ()
runModule (Module statements _) = runStatements statements

runStatements :: Eq a => [Statement a] -> Interpreter a ()
runStatements = mapM_ runStatement

runStatement :: Eq a => Statement a -> Interpreter a ()
runStatement s_ = case s_ of
    Definition name _ rhs _ -> do
        value <- evalExpr rhs
        modify $ pushStack name value
    Assignment name rhs _ -> do
        value <- evalExpr rhs
        modify $ updateStack name value
    Execution e _ -> do
        void (evalExpr e)
    If cnd thn m_els tag -> do
        cnd_value <- evalExpr cnd
        case cnd_value of
            CBool True -> runStatements thn
            CBool False -> mapM_ runStatements m_els
            _ -> throw $ InternalError ("condition not a boolean at runtime: "++show cnd) tag
    While cnd body tag -> do
        cnd_value <- evalExpr cnd
        case cnd_value of
            CBool True -> runStatements body >> runStatement s_ -- run the while loop again
            CBool False -> return ()
            _ -> throw $ InternalError ("condition not a boolean at runtime: "++show cnd) tag

evalExpr :: Eq a => Expr a -> Interpreter a Cell
evalExpr e_ = case e_ of
    Lit l_ _ -> return $ case l_ of
        LInt n _ -> CInt n
        LDouble d _ -> CDouble d
        LChar c _ -> CChar c
        LBool b _ -> CBool b
    Var name tag -> do
        mem <- get
        case stackLookup mem name of
            Nothing -> throw $ InternalError ("unbound var at runtime: "++show name) tag
            Just c -> return c
    Paren e _ -> evalExpr e

runProgramWith :: Eq a => Module a -> InterpreterState a -> Either (DynamicError a, InterpreterState a) ((), InterpreterState a)
runProgramWith m = runStateT (runModule m)

runProgram :: Eq a => Module a -> Either (DynamicError a, InterpreterState a) ((), InterpreterState a)
runProgram m = runProgramWith m emptyMemory
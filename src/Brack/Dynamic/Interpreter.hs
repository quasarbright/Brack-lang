module Brack.Dynamic.Interpreter where

import Brack.Utils.Common

import Brack.Dynamic.Memory
import Brack.Dynamic.DynamicError

--import Brack.Syntax.Name
import Brack.Syntax.Name
import Brack.Syntax.Type
import Brack.Syntax.Statement
import Brack.Syntax.Module

import Control.Monad
--import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Fixed (mod')

import qualified Data.Map as Map

type InterpreterState a = Memory a

type Interpreter a b = StateExceptT (InterpreterState a) (DynamicError a) b

-- TODO think about this more. Maybe there's a monad here
-- what if statement itself could be a monad?
-- maybe everything should just return a Result. it'll be like "answer goes in rax"
    -- good for try catch
-- | The result of a computation
data Result = Returned Cell
            | Broke
            | Continued
            -- TODO errors
            | Normal
            deriving(Eq, Ord, Show)

runModule :: Ord a => Module a -> Interpreter a Result
runModule (Module statements tag) = do
    modify (pushFrame (Main tag))
    runStatements statements

runStatements :: Ord a => [Statement a] -> Interpreter a Result
runStatements [] = return Normal
runStatements (statement:statements) = do
    result <- runStatement statement
    case result of
        Normal -> runStatements statements -- keep going
        _ -> return result -- stop

runBlock :: Ord a => [Statement a] -> Interpreter a Result
runBlock statements = do
    modify pushScope
    result <- runStatements statements
    modify popScope
    return result

runStatement :: Ord a => Statement a -> Interpreter a Result
runStatement s_ = case s_ of
    Definition name _ rhs _ -> do
        value <- evalExpr rhs
        modify $ pushVariable name value
        return Normal
    FunctionDefinition name typedArgs retType body tag -> do
        value <- allocateFunction (Just name) typedArgs retType body tag
        modify $ pushVariable name value
        return Normal
    Assignment name rhs _ -> do
        value <- evalExpr rhs
        modify $ updateVariable name value
        return Normal
    Execution e _ -> do
        void (evalExpr e)
        return Normal
    If cnd thn m_els tag -> do
        cnd_value <- evalExpr cnd
        case cnd_value of
            CBool True -> runBlock thn
            CBool False ->
                case m_els of
                    Nothing -> return Normal
                    Just els -> runBlock els
            _ -> throw $ InternalError ("condition not a boolean at runtime: "++show cnd) tag
    While cnd body tag -> do
        cnd_value <- evalExpr cnd
        case cnd_value of
            CBool True -> do
                result <- runBlock body
                case result of
                    Normal -> runStatement s_ -- run the while loop again
                    Continued -> runStatement s_ -- run the while loop again
                    Broke -> return result
                    Returned{} -> return result
            CBool False -> return Normal
            _ -> throw $ InternalError ("condition not a boolean at runtime: "++show cnd) tag
    Return e _ -> Returned <$> evalExpr e
    Break _ -> return Broke
    Continue _ -> return Continued


allocateFunction :: Ord a => Maybe (QName a) -> [(QName a, Type a)] -> Type a -> [Statement a] -> a -> Interpreter a Cell
allocateFunction mName typedArgs retType body tag = do
    locals <- getLocals <$> get -- TODO free variables only
    let value = Closure mName typedArgs retType body locals tag
    (mem', addr) <- heapAlloc value <$> get
    put mem'
    return (Pointer addr)

evalExpr :: Ord a => Expr a -> Interpreter a Cell
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
    Prim1 Negate e tag -> do
        value <- evalExpr e
        case value of
            CInt n -> return (CInt (-n))
            CDouble d -> return (CDouble (-d))
            _ -> throw $ InternalError ("negated non-number" ++ show e) tag
    Prim1 Not e tag -> do
        value <- evalExpr e
        case value of
            CBool b -> return (CBool (not b))
            _ -> throw $ InternalError ("not-ed non-bool" ++ show e) tag
    Prim2 left And right tag -> do
        lval <- evalExpr left
        case lval of
            CBool True -> evalExpr right
            CBool False -> return lval
            _ -> throw $ InternalError ("and-ed non-bool" ++ show left) tag
    Prim2 left Or right tag -> do
        lval <- evalExpr left
        case lval of
            CBool False -> evalExpr right
            CBool True -> return lval
            _ -> throw $ InternalError ("or-ed non-bool" ++ show left) tag
    Prim2 left prim2 right tag -> do
        lval <- evalExpr left
        rval <- evalExpr right
        let promoteNumbers a b =
                case (a,b) of
                    (CInt n, CDouble{}) -> (CDouble (fromInteger n), b)
                    (CDouble{}, CInt n) -> (a, CDouble (fromInteger n))
                    _ -> (a,b)
        let (lval', rval') = promoteNumbers lval rval
        case (lval', prim2, rval') of
            (CInt a, Plus, CInt b) -> return (CInt (a + b))
            (CDouble a, Plus, CDouble b) -> return (CDouble (a + b))
            (CInt a, Minus, CInt b) -> return (CInt (a - b))
            (CDouble a, Minus, CDouble b) -> return (CDouble (a - b))
            (CInt a, Times, CInt b) -> return (CInt (a * b))
            (CDouble a, Times, CDouble b) -> return (CDouble (a * b))
            (_, Divide, CInt 0) -> throw (DivideByZero tag)
            (CInt a, Divide, CInt b) -> return (CInt (a `div` b))
            (_, Divide, CDouble 0.0) -> throw (DivideByZero tag)
            (CDouble a, Divide, CDouble b) -> return (CDouble (a / b))
            (_, Modulo, CInt 0) -> throw (DivideByZero tag)
            (CInt a, Modulo, CInt b) -> return (CInt (a `mod` b))
            (_, Modulo, CDouble 0.0) -> throw (DivideByZero tag)
            (CDouble a, Modulo, CDouble b) -> return (CDouble (a `mod'` b))
            (CInt a, Pow, CInt b)
                | a == 0 && b < 0 -> throw (DivideByZero tag)
                | otherwise -> return (CDouble (fromIntegral a ** fromIntegral b))
            (CDouble a, Pow, CDouble b)
                | a == 0 && b < 0 -> throw (DivideByZero tag)
                | otherwise -> return (CDouble (a ** b))
            (CInt a, Less, CInt b) -> return (CBool (a < b))
            (CDouble a, Less, CDouble b) -> return (CBool (a < b))
            (CInt a, Greater, CInt b) -> return (CBool (a > b))
            (CDouble a, Greater, CDouble b) -> return (CBool (a > b))
            (CInt a, LessEq, CInt b) -> return (CBool (a <= b))
            (CDouble a, LessEq, CDouble b) -> return (CBool (a <= b))
            (CInt a, GreaterEq, CInt b) -> return (CBool (a >= b))
            (CDouble a, GreaterEq, CDouble b) -> return (CBool (a >= b))
            (a, Equals, b) -> return (CBool (a == b))
            (a, NotEquals, b) -> return (CBool (a /= b))
            _ -> throw $ InternalError ("uncaught prim2 case " ++ show e_) tag
    Application f args tag -> do
        fVal <- evalExpr f
        argVals <- mapM evalExpr args
        case fVal of
            Pointer addr -> do
                mBoxed <- heapLookup <$> get <*> return addr
                case mBoxed of
                    Just boxed@(Closure _ typedArgs _ body locals _) -> do
                        modify $ pushFrame (Call boxed tag)
                        let argNames = fst <$> typedArgs
                        unless (length args == length argNames) (throw (InternalError "arity error" tag))
                        let locals' = Map.union (Map.fromList (zip argNames argVals)) locals
                        mapM_ (\(name, val) -> modify $ pushVariable name val) (Map.toList locals')
                        result <- runBlock body
                        modify popFrame
                        case result of
                            Returned value -> return value
                            _ -> throw (InternalError "function did not return" tag)
                    Just _ -> throw (InternalError "applied non-function" tag)
                    _ -> throw (InternalError "couldn't find function in heap" tag)
            _ -> throw (InternalError "applied non-function" tag)
    Function typedArgs retType body tag -> allocateFunction Nothing typedArgs retType body tag
    Paren e _ -> evalExpr e

runProgramWith :: Ord a => Module a -> InterpreterState a -> Either (DynamicError a, InterpreterState a) (Result, InterpreterState a)
runProgramWith m = runStateT (runModule m)

runProgram :: Ord a => Module a -> Either (DynamicError a, InterpreterState a) (Result, InterpreterState a)
runProgram m = runProgramWith m emptyMemory
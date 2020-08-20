module Brack.Static.TypeCheck where

import Brack.Syntax.Name
import Brack.Syntax.Type
import Brack.Syntax.Statement
import Brack.Syntax.Module

import Brack.Static.StaticError
import Brack.Utils.Common

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad

import Data.Map(Map)
import qualified Data.Map as Map

data TCState a = MkState { getCurrentTag :: a, getContext :: Map (QName a) (Type a)} deriving(Eq, Ord, Show)

initialState :: a -> TCState a
initialState a = MkState { getCurrentTag = a, getContext = Map.empty }

type TypeChecker a b = StateT (TCState a) (Either (StaticError a, TCState a)) b

throwAndTag :: (a -> StaticError a) -> TypeChecker a b
throwAndTag errFun = do
    s <- get
    let tag = getCurrentTag s
    lift (Left (errFun tag, s))

throw :: StaticError a -> TypeChecker a b
throw err = do
    s <- get
    lift (Left (err, s))

localTagged :: Tagged t => t a -> TypeChecker a b -> TypeChecker a b
localTagged t = localTag (getTag t)

localTag :: a -> TypeChecker a b -> TypeChecker a b
localTag newTag computation = do
    s <- get
    let oldTag = getCurrentTag s
    put (s{getCurrentTag = newTag})
    result <- computation
    s' <- get
    put (s'{getCurrentTag = oldTag})
    return result

checkModule :: Ord a => Module a -> TypeChecker a ()
checkModule (Module statements tag) = checkStatements statements (TVoid tag)

checkStatements :: Ord a => [Statement a] -> Type a -> TypeChecker a ()
checkStatements statements t = do
    oldCtx <- getContext <$> get
    mapM_ (`checkStatement` t) statements
    s <- get
    put (s{getContext = oldCtx})

checkStatement :: Ord a => Statement a -> Type a -> TypeChecker a ()
checkStatement s_ t = localTagged s_ $
    case s_ of
        Definition name t' e _ -> do
            checkExpr e t'
            s <- get
            let ctx = getContext s
            let ctx' = Map.insert name t' ctx
            put (s{getContext = ctx'})
        FunctionDefinition name typedArgs retType body tag -> do
            funType <- inferFunType typedArgs retType body tag
            modify $ \s -> s{getContext = Map.insert name funType (getContext s)}
        Assignment name e tag -> do
            nameType <- inferExpr (Var name tag)
            checkExpr e nameType
        Execution e _ -> void (inferExpr e)
        If cnd thn m_els tag -> do
            checkExpr cnd (TBool tag)
            checkStatements thn t
            case m_els of
                Nothing -> return ()
                Just els -> checkStatements els t
        While cnd body tag -> do
            checkExpr cnd (TBool tag)
            checkStatements body t
        Return e _ -> checkExpr e t
        Break{} -> return ()
        Continue{} -> return ()

inferFunType :: Ord a => [(QName a, Type a)] -> Type a -> [Statement a] -> a -> TypeChecker a (Type a)
inferFunType typedArgs retType body tag = do
    oldCtx <- getContext <$> get
    let newCtx = Map.union (Map.fromList typedArgs) oldCtx
    modify $ \s -> s{getContext = newCtx}
    checkStatements body retType
    modify $ \s -> s{getContext = oldCtx}
    return $ TArr (snd <$> typedArgs) retType tag

checkExpr :: Ord a => Expr a -> Type a -> TypeChecker a ()
checkExpr e t = localTagged e $ do
    t' <- inferExpr e
    assertSameType t t'

inferExpr :: Ord a => Expr a -> TypeChecker a (Type a)
inferExpr e_ = localTagged e_ $ case e_ of
    Var name _ -> do
        ctx <- getContext <$> get
        case Map.lookup name ctx of
            Nothing -> throwAndTag (UnboundVar name)
            Just t -> return t
    Lit l_ tag -> return $ case l_ of
       LInt{} -> TInt tag
       LDouble{} -> TDouble tag
       LChar{} -> TChar tag
       LBool{} -> TBool tag
    Prim1 prim1 e tag ->
        case prim1 of
            Not -> checkExpr e (TBool tag) >> return (TBool tag)
            Negate -> do
                t <- inferExpr e
                case t of
                    TInt{} -> return t
                    TDouble{} -> return t
                    _ -> throwAndTag (TypeMismatch (TDouble tag) t)
    Prim2 left prim2 right tag ->
        let doArith = inferArith left right tag
            doCmp = inferCmp left right tag
            doLogic = inferLogic left right tag
        in case prim2 of
            Plus -> doArith
            Minus -> doArith
            Times -> doArith
            Divide -> doArith
            Modulo -> doArith
            Pow -> doArith >> return (TDouble tag)
            Less -> doCmp
            Greater -> doCmp
            LessEq -> doCmp
            GreaterEq -> doCmp
            Equals -> inferExpr left >> inferExpr right >> return (TBool tag)
            NotEquals -> inferExpr left >> inferExpr right >> return (TBool tag)
            Or -> doLogic
            And -> doLogic
    Application f args _ -> do
        fType <- inferExpr f
        case fType of
            TArr argTypes retType _ -> do
                unless (length argTypes == length args) (throwAndTag (ArityError (length argTypes) (length args)))
                zipWithM_ checkExpr args argTypes
                return retType
            _ -> throwAndTag (AppliedNonFunction fType)
    Function typedArgs retType body tag -> inferFunType typedArgs retType body tag
    Paren e _ -> inferExpr e

inferArith :: Ord a => Expr a -> Expr a -> a -> TypeChecker a (Type a)
inferArith left right tag = do
    lType <- inferExpr left
    rType <- inferExpr right
    case (lType, rType) of
        (TDouble{}, TDouble{}) -> return lType
        (TDouble{}, TInt{}) -> return lType
        (TInt{}, TDouble{}) -> return rType
        (TInt{}, TInt{}) -> return lType
        (TInt{}, _) -> throwAndTag (TypeMismatch lType rType)
        (TDouble{}, _) -> throwAndTag (TypeMismatch lType rType)
        (_, TInt{}) -> throwAndTag (TypeMismatch rType lType)
        (_, TDouble{}) -> throwAndTag (TypeMismatch rType lType)
        _ -> throwAndTag (TypeMismatch (TDouble tag) lType)

inferCmp :: Ord a => Expr a -> Expr a -> a -> TypeChecker a (Type a)
inferCmp left right tag = do
    lType <- inferExpr left
    rType <- inferExpr right
    let tBool = TBool tag
    case (lType, rType) of
        (TDouble{}, TDouble{}) -> return tBool
        (TDouble{}, TInt{}) -> return tBool
        (TInt{}, TDouble{}) -> return tBool
        (TInt{}, TInt{}) -> return tBool
        (TInt{}, _) -> throwAndTag (TypeMismatch lType rType)
        (TDouble{}, _) -> throwAndTag (TypeMismatch lType rType)
        (_, TInt{}) -> throwAndTag (TypeMismatch rType lType)
        (_, TDouble{}) -> throwAndTag (TypeMismatch rType lType)
        _ -> throwAndTag (TypeMismatch (TDouble tag) lType)

inferLogic :: Ord a => Expr a -> Expr a -> a -> TypeChecker a (Type a)
inferLogic left right tag = do
    checkExpr left (TBool tag)
    checkExpr right (TBool tag)
    return (TBool tag)

assertSameType :: Eq a => Type a -> Type a -> TypeChecker a ()
assertSameType expected actual =
    let err = throwAndTag (TypeMismatch expected actual)
        ok  = return () :: TypeChecker a ()
    in case (expected, actual) of
        (TInt{}, TInt{}) -> ok
        (TChar{}, TChar{}) -> ok
        (TBool{}, TBool{}) -> ok
        (TDouble{}, TDouble{}) -> ok
        (TVoid{}, TVoid{}) -> ok
        (TCon a _, TCon b _) -> unless (a == b) err
        (TArr args ret _, TArr args' ret' _) -> zipWithM_ assertSameType (args++[ret]) (args'++[ret'])
        (TInt{},_) -> err
        (TChar{},_) -> err
        (TDouble{},_) -> err
        (TBool{},_) -> err
        (TVoid{},_) -> err
        (TCon{},_) -> err
        (TArr{}, _) -> err

runTypeCheckingWith :: Ord a => Module a -> TCState a -> Either (StaticError a, TCState a) ((), TCState a)
runTypeCheckingWith m = runStateT (checkModule m)

runTypeChecking :: Ord a => Module a -> Either (StaticError a, TCState a) ((), TCState a)
runTypeChecking m@(Module _ a) = runTypeCheckingWith m (initialState a)
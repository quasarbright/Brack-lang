module Brack.Parsing.Parser where


import Brack.Utils.Common
import Brack.Syntax.Name
import Brack.Syntax.Type
import Brack.Syntax.Expr
import Brack.Syntax.Statement
import Brack.Syntax.Module

import Control.Applicative hiding (some, many)
import Data.Functor(($>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Brack.Parsing.ParseUtils
import Data.Void (Void)

-- expressions
type ExprParser = Parser (Expr SS) -> Parser (Expr SS)

pExpr :: Parser (Expr SS)
pExpr = topParser [pOp, pAtomic]

pOp :: ExprParser
pOp child = makeExprParser child table
    where
        table =
            [ [Prefix (prim1Chain Negate)]
            , [Prefix (prim1Chain Not)]
            , [InfixR (prim2Chain Pow)]
            , [InfixL (prim2Chain Times), InfixL (prim2Chain Divide), InfixL (prim2Chain Modulo)]
            , [InfixL (prim2Chain Plus), InfixL (prim2Chain Minus)]
            , [InfixL (prim2Chain prim2) | prim2 <- [Less, LessEq, Greater, GreaterEq]]
            , [InfixL (prim2Chain Equals), InfixL (prim2Chain NotEquals)]
            , [InfixL (prim2Chain Or)]
            , [InfixL (prim2Chain And)]
            ]

prim2Chain :: Prim2 -> Parser (Expr SS -> Expr SS -> Expr SS)
prim2Chain prim2 = pReservedOp (show prim2) >> return f
    where f left right = Prim2 left prim2 right (combineSS (getTag left) (getTag right))

prim1Chain :: Prim1 -> Parser (Expr SS -> Expr SS)
prim1Chain prim1 = do
    startPos <- getSourcePos
    pReservedOp (show prim1)
    endPos <- getSourcePos
    let ss = (startPos, endPos)
    return $ \e -> Prim1 prim1 e (combineSS ss (getTag e))

pAtomic :: ExprParser
pAtomic child = choice [wrapSSApp (Lit <$> pLiteral), pVar, pParen child]

pParen :: ExprParser
pParen child = wrapSSApp (Paren <$> parens child)

pVar :: Parser (Expr SS)
pVar = wrapSSApp $ Var <$> pLowerName

pLowerName :: Parser (QName SS)
pLowerName = do
    startPos <- getSourcePos
    name <- lowerIdentifier
    endPos <- getSourcePos
    let ss = (startPos, endPos)
    return (UnQualified (LowerIdent name ss) ss)

pUpperName :: Parser (QName SS)
pUpperName = do
    startPos <- getSourcePos
    name <- upperIdentifier
    endPos <- getSourcePos
    let ss = (startPos, endPos)
    return (UnQualified (UpperIdent name ss) ss)

pLiteral :: Parser (Literal SS)
pLiteral = choice [pChar, try pDouble, pInt, pBool]

pChar :: Parser (Literal SS)
pChar = wrapSSApp $ LChar <$> lexeme (between quote quote L.charLiteral)
    where quote = char '\''

pInt :: Parser (Literal SS)
pInt = wrapSSApp $ LInt <$> lexeme L.decimal

pDouble :: Parser (Literal SS)
pDouble = wrapSSApp $ LDouble <$> lexeme L.float

pBool :: Parser (Literal SS)
pBool = wrapSSApp $ LBool <$> choice [pKeyword "true" $> True, pKeyword "false" $> False]

-- types

pType :: Parser (Type SS)
pType = choice (pConType : (uncurry pSpecialType <$> [("int", TInt), ("double", TDouble), ("char", TChar), ("bool", TBool), ("void", TVoid)]))

pSpecialType :: String -> (SS -> Type SS) -> Parser (Type SS)
pSpecialType name con = wrapSSApp (pKeyword name $> con)

pConType :: Parser (Type SS)
pConType = wrapSSApp $ TCon <$> pUpperName

-- statements

pSemi :: Parser ()
pSemi = pReservedOp ";"

block :: Parser [Statement SS]
block = braces (many pStatement)

blockOrSingle :: Parser [Statement SS]
blockOrSingle = block <|> ((: []) <$> pStatement)

pStatement :: Parser (Statement SS)
pStatement = choice [pIf, pWhile, try pAssignment, try pDefinition, pExecution]

pDefinition :: Parser (Statement SS)
pDefinition = wrapSSApp $ do
    name <- pLowerName
    pReservedOp "::"
    typ <- pType
    pReservedOp "="
    rhs <- pExpr
    pReservedOp ";"
    return (Definition name typ rhs)

pAssignment :: Parser (Statement SS)
pAssignment = wrapSSApp $ do
    name <- pLowerName
    pReservedOp "="
    rhs <- pExpr
    pReservedOp ";"
    return (Assignment name rhs)

pExecution :: Parser (Statement SS)
pExecution = wrapSSApp $ Execution <$> (pExpr <* pReservedOp ";")

pIf :: Parser (Statement SS)
pIf = wrapSSApp $ do
    pKeyword "if"
    condition <- parens pExpr
    thn <- blockOrSingle
    m_els <- optional (pKeyword "else" *> blockOrSingle)
    return (If condition thn m_els)

pWhile :: Parser (Statement SS)
pWhile = wrapSSApp $ pKeyword "while" *> (While <$> parens pExpr <*> blockOrSingle)

-- module

pModule :: Parser (Module SS)
pModule = between scn eof (wrapSSApp $ Module <$> many pStatement)

parseModule :: String -> String -> Either (ParseErrorBundle String Void) (Module SS)
parseModule = runParser pModule

parseModuleUnsafe :: String -> String -> Module SS
parseModuleUnsafe name input = case parseModule name input of
    Left err -> error (show err)
    Right m -> m
module Brack.Parsing.Parser where

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

import Brack.Parsing.ParseUtils

-- expressions

pExpr :: Parser (Expr SS)
pExpr = choice [wrapSSApp (Lit <$> pLiteral), pVar]

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
pType = choice (pConType : (uncurry pSpecialType <$> [("int", TInt), ("double", TDouble), ("char", TChar), ("void", TVoid)]))

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
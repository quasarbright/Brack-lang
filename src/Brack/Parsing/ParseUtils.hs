{-# LANGUAGE TypeFamilies #-}
module Brack.Parsing.ParseUtils where

import Control.Applicative hiding (some, many)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "/*" "*/"

-- | whitespace consumer that consumes newlines
scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- | whitespace consumer that doesn't consume newlines
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: String -> Parser ()
symbol = void . L.symbol scn

pKeyword :: String -> Parser ()
pKeyword = pKeywordWith scn

pKeywordWith :: Parser () -> String -> Parser ()
pKeywordWith sc' word = void . L.lexeme sc' $ (string word <* notFollowedBy identLetter)

pReservedOp :: String -> Parser ()
pReservedOp = pReservedOpWith scn

pReservedOpWith :: Parser () -> String -> Parser ()
pReservedOpWith sc' name = void . L.lexeme sc' $ (string name <* notFollowedBy opLetter)

reservedWords :: [String]
reservedWords = words "if else int char bool double true false public private const var for while do"

reservedOps :: [String]
reservedOps = words "= -> => :: || && + - * / ; ^"

lowerIdentStart :: Parser Char
lowerIdentStart = lowerChar <|> char '_'

upperIdentStart :: Parser Char
upperIdentStart = upperChar

identLetter :: Parser Char
identLetter = choice [lowerIdentStart, upperIdentStart, numberChar]

_identifier :: Parser Char -> Parser String
_identifier start = (lexeme . try) (p >>= check)
 where
   p       = (:) <$> start <*> many identLetter
   check x =
     if x `elem` reservedWords
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x

lowerIdentifier :: Parser String
lowerIdentifier = _identifier lowerIdentStart

upperIdentifier :: Parser String
upperIdentifier = _identifier upperIdentStart

opLetter :: Parser Char
opLetter = oneOf "\\|/-+=!@#$%^&*~.?<>"

operator :: Parser String
operator = operatorWith sc

operatorWith :: Parser () -> Parser String
operatorWith sc' = (L.lexeme sc' . try) (p >>= check)
   where
       p = some opLetter
       check x =
           if x `elem` reservedOps
           then fail $ "reserved operator " ++ show x ++ " cannot be an identifier"
           else return x

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

topParser :: [Parser a -> Parser a] -> Parser a
topParser parsers = foldr (\ep p -> ep p) (error "you will never arrive at the truth") (cycle parsers)

type SS = (SourcePos, SourcePos)

dummySS :: SS
dummySS = (sp, sp) where sp = SourcePos "<dummy>" (mkPos 1) (mkPos 1)

-- | run the given parser and record the source span of the parse
wrapSS :: Parser a -> Parser (a, SS)
wrapSS p = do
    startPos <- getSourcePos
    result <- p
    endPos <- getSourcePos
    return (result, (startPos, endPos))

-- | use like this:
-- wrapSSApp $ do
--   n <- L.decimal <* scn
--   return $ EInt n
-- It just calls what you return with the ss!
wrapSSApp :: Parser (SS -> a) -> Parser a
wrapSSApp p = uncurry ($) <$> wrapSS p

combineSS :: (a1, b1) -> (a2, b2) -> (a1, b2)
combineSS a b = (fst a, snd b)

-- | optional with backtracking
vot :: Parser () -> Parser ()
vot = void . try . optional

-- | copied from the parsec package:
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/src/Text.Parsec.Combinator.html#chainl1
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <|> return x
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op        = scan
                    where
                      scan      = do{ x <- p; rest x }
                      rest x    = do f <- op
                                     f x <$> scan
                                <|> return x
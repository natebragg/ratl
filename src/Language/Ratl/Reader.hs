module Language.Ratl.Reader (
    sexp,
    sexps,
) where

import Language.Ratl.Ast (
    Embeddable(..),
    Val(..),
    Sym(..),
    )

import Data.Char (isSpace)
import Text.Parsec (try, many, (<|>), (<?>))
import Text.Parsec.Char (noneOf, char)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.Token (GenLanguageDef(..))
import qualified Text.Parsec.Token as P
import Control.Monad (mzero, mfilter)


readerStyle :: LanguageDef st
readerStyle = emptyDef {
                commentLine    = ";",
                nestedComments = True,
                identStart     = identLetter readerStyle,
                identLetter    = try $ mfilter (not . isSpace) $ noneOf "()[];",
                opStart        = opLetter readerStyle,
                opLetter       = mzero,
                reservedOpNames= [],
                reservedNames  = [],
                caseSensitive  = True
            }

lexer = P.makeTokenParser readerStyle

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

identifier :: Parser String
identifier = P.identifier lexer

num :: Parser Integer
num = P.lexeme lexer (P.decimal lexer)

nat :: Parser Int
nat = fromInteger <$> num

boolean :: Parser Bool
boolean = (reserved "#t" >> return True)
      <|> (reserved "#f" >> return False)

sym :: Parser Sym
sym = S <$> identifier

atom :: Parser Val
atom = embed <$> boolean
   <|> embed <$> nat
   <|> embed <$> sym
   <?> "atom"

list :: Parser Val
list = embed <$> parens (many sexp)
   <|> embed <$> brackets (many sexp)
   <?> "list"

sexp :: Parser Val
sexp = embed <$> ((embed (S "quote") :) <$> (pure <$> (char '\'' >> sexp)))
   <|> atom
   <|> list
   <?> "s-expression"

sexps :: Parser Val
sexps = whiteSpace >> embed <$> many sexp

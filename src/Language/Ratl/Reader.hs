module Language.Ratl.Reader (
    sexp,
    sexps,
) where

import Language.Ratl.Val (
    Embeddable(..),
    Span(Span),
    Lit(..),
    litList,
    withSpan,
    )

import Data.Char (isSpace)
import Text.Parsec (try, many, getPosition, (<|>), (<?>))
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

group :: Parser a -> Parser a
group = (<|>) <$> parens <*> brackets

nat :: Parser Int
nat = fromInteger <$> num

boolean :: Parser Bool
boolean = (reserved "#t" >> return True)
      <|> (reserved "#f" >> return False)

sym :: Parser String
sym = identifier

spanOf :: (Span -> a -> Lit) -> Parser a -> Parser Lit
spanOf c p = do
    pos <- getPosition
    result <- p
    pos' <- getPosition
    return $ c (Span pos pos') result

atom :: Parser Lit
atom = spanOf LitBoolean boolean
   <|> spanOf LitNat nat
   <|> spanOf LitSym sym
   <?> "atom"

list :: Parser Lit
list = spanOf withSpan (group $ litList <$> many sexp)
   <?> "list"

quoted :: Parser Lit
quoted = spanOf withSpan $ do
    quote <- spanOf LitSym $ char '\'' >> return "quote"
    datum <- sexp
    return $ litList [quote, datum]

sexp :: Parser Lit
sexp = quoted
   <|> atom
   <|> list
   <?> "s-expression"

sexps :: Parser Lit
sexps = whiteSpace >> spanOf withSpan (litList <$> many sexp)

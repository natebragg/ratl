module Language.Ratl.Parser (
    prog,
    val,
) where

import Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Boolean(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog,
    makeProg,
    )
import Language.Ratl.Ty (
    Ty(..),
    FunTy(..),
    )

import Data.Char (isSpace, isDigit)
import Text.Parsec (try, many, count, sepEndBy, (<|>), (<?>))
import Text.Parsec.Char (noneOf)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.Token (GenLanguageDef(..))
import qualified Text.Parsec.Token as P
import Control.Monad (mzero, mfilter)



lispStyle :: LanguageDef st
lispStyle = emptyDef {
                commentLine    = ";",
                nestedComments = True,
                identStart     = try $ mfilter (not . isDigit) $ identLetter lispStyle,
                identLetter    = try $ mfilter (not . isSpace) $ noneOf "()[];,",
                opStart        = opLetter lispStyle,
                opLetter       = mzero,
                reservedOpNames= ["->"],
                reservedNames  = ["define", "if", "let",
                                  "#t", "#f",
                                  "Nat", "Boolean"],
                caseSensitive  = True
            }

lexer = P.makeTokenParser lispStyle

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

comma :: Parser ()
comma = P.comma lexer >> return ()

symbol :: String -> Parser String
symbol = P.symbol lexer

num :: Parser Integer
num = P.lexeme lexer (P.decimal lexer)

nat :: Parser Nat
nat = embed <$> fromInteger <$> num

list :: Parser List
list = embed <$> brackets (sepEndBy val comma)
   <?> "list"

boolean :: Parser Boolean
boolean = embed <$> ((reserved "#t" >> return True)
                 <|> (reserved "#f" >> return False))

var :: Parser Var
var = V <$> identifier

val :: Parser Val
val = List <$> list
  <|> Boolean <$> boolean
  <|> Nat <$> nat
  <?> "value"

ex :: Parser Ex
ex = Val <$> val
 <|> Var <$> var
 <|> parens ((reserved "if" >> If <$> ex <*> ex <*> ex)
         <|> (reserved "let" >> Let <$> parens (many $ parens $ (,) <$> var <*> ex) <*> ex)
         <|> try (App <$> var <*> many ex))

ty :: Parser (Ty ())
ty = (reserved "Nat" >> return NatTy)
  <|> (reserved "Boolean" >> return BooleanTy)
  <|> ListTy [] <$> brackets ty
  <?> "type"

funty :: Parser (FunTy ())
funty = do t1 <- ty
           t2 <- reservedOp "->" >> ty
           return $ Arrow ((), ()) [t1] t2

fun :: Parser (Var, Fun ())
fun = parens (reserved "define" >>
              (,) <$> var
                  <*> (Fun <$> parens funty
                           <*> parens var
                           <*> ex))
  <?> "function definition"

prog :: Parser (Prog ())
prog = whiteSpace >> makeProg <$> many fun

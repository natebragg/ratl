module Language.Ratl.Parser (
    prog,
    val,
) where

import Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog
    )
import Language.Ratl.Ty (
    Ty(..),
    FunTy(..),
    )
import Language.Ratl.Basis (arity)

import Text.Parsec (try, many1, count, sepEndBy, (<|>))
import Text.Parsec.Char (char, lower)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.Token (GenLanguageDef(..))
import qualified Text.Parsec.Token as P
import Control.Monad (mzero)

lispStyle :: LanguageDef st
lispStyle = emptyDef {
                commentLine    = ";",
                nestedComments = True,
                identStart     = identLetter lispStyle,
                identLetter    = char '_' <|> lower,
                opStart        = opLetter lispStyle,
                opLetter       = mzero,
                reservedOpNames= [],
                reservedNames  = [],
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

var :: Parser Var
var = V <$> identifier

val :: Parser Val
val = List <$> list <|> Nat <$> nat

ex :: Parser Ex
ex = Val <$> val
 <|> Var <$> var
 <|> parens (try $ do v <- V <$> symbol "+" <|> var
                      es <- count (arity v) ex
                      return $ App v es
             <|> ex)

ty :: Parser (Ty ())
ty = (reserved "Nat" >> return NatTy)
  <|> ListTy () <$> brackets ty

funty :: Parser (FunTy ())
funty = do t1 <- ty
           t2 <- reservedOp "->" >> ty
           return $ Arrow () [t1] t2

fun :: Parser (Var, Fun ())
fun = parens (reserved "fn" >>
              (,) <$> var
                  <*> (Fun <$> parens funty
                           <*> parens var
                           <*> ex))

prog :: Parser (Prog ())
prog = whiteSpace >> many1 fun

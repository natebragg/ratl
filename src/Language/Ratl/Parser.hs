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

boolean :: Parser Boolean
boolean = embed <$> ((reserved "#t" >> return True)
                 <|> (reserved "#f" >> return False))

var :: Parser Var
var = V <$> identifier

val :: Parser Val
val = List <$> list
  <|> Boolean <$> boolean
  <|> Nat <$> nat

ex :: Parser Ex
ex = Val <$> val
 <|> Var <$> var
 <|> parens ((reserved "if" >> If <$> ex <*> ex <*> ex)
         <|> try (do v <- V <$> symbol "+" <|> V <$> symbol "<" <|> var
                     es <- count (arity v) ex
                     return $ App v es)
         <|> ex)

ty :: Parser (Ty ())
ty = (reserved "Nat" >> return NatTy)
  <|> (reserved "Boolean" >> return BooleanTy)
  <|> ListTy [] <$> brackets ty

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

prog :: Parser (Prog ())
prog = whiteSpace >> makeProg <$> many1 fun

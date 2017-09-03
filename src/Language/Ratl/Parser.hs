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

import Text.Parsec (try, many1, count, sepEndBy, between, (<|>))
import Text.Parsec.Char (char, string, digit, space, spaces, lower)
import Text.Parsec.String (Parser)

spaces1 :: Parser ()
spaces1 = space >> spaces

parens :: Parser a -> Parser a
parens = between (char '(' >> spaces) (spaces >> char ')')

brackets :: Parser a -> Parser a
brackets = between (char '[' >> spaces) (spaces >> char ']')

identifier :: Parser String
identifier = many1 (char '_' <|> lower)

nat :: Parser Nat
nat = embed <$> read <$> many1 digit

list :: Parser List
list = embed <$> brackets (sepEndBy val (spaces >> char ',' >> spaces))

var :: Parser Var
var = V <$> identifier

val :: Parser Val
val = List <$> list <|> Nat <$> nat

ex :: Parser Ex
ex = Val <$> val
 <|> Var <$> var
 <|> parens (try $ do v <- V <$> string "+" <|> var
                      es <- count (arity v) (spaces1 >> ex)
                      return $ App v es
             <|> ex)

ty :: Parser (Ty ())
ty = try (string "Nat" >> return NatTy)
  <|> ListTy () <$> brackets ty

funty :: Parser (FunTy ())
funty = do t1 <- ty
           t2 <- (spaces1 >> string "->" >> spaces1 >> ty)
           return $ Arrow () t1 t2

fun :: Parser (Var, Fun ())
fun = spaces >>
      parens (string "fn" >>
              (,) <$> (spaces1 >> var)
                  <*> (Fun <$> (spaces1 >> parens funty)
                           <*> (spaces1 >> parens var)
                           <*> (spaces1 >> ex))) <* spaces

prog :: Parser (Prog ())
prog = many1 fun

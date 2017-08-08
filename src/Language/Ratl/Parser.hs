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
    Anno,
    Ty(..),
    FunTy(..),
    )
import Language.Ratl.Elab (
    freshFunTy,
    freshListTy,
    )

import Text.Parsec (ParsecT)
import Text.Parsec (try, many1, sepEndBy, between, (<|>))
import Text.Parsec.Char (char, string, digit, space, spaces, lower)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)

type Parser m a = ParsecT String () (StateT Anno m) a

spaces1 :: Monad m => Parser m ()
spaces1 = space >> spaces

parens :: Monad m => Parser m a -> Parser m a
parens = between (char '(' >> spaces) (spaces >> char ')')

brackets :: Monad m => Parser m a -> Parser m a
brackets = between (char '[' >> spaces) (spaces >> char ']')

identifier :: Monad m => Parser m String
identifier = many1 (char '_' <|> lower)

nat :: Monad m => Parser m Nat
nat = embed <$> read <$> many1 digit

list :: Monad m => Parser m List
list = embed <$> brackets (sepEndBy val (spaces >> char ',' >> spaces))

var :: Monad m => Parser m Var
var = V <$> identifier

val :: Monad m => Parser m Val
val = List <$> list <|> Nat <$> nat

ex :: Monad m => Parser m Ex
ex = Val <$> val
 <|> Var <$> var
 <|> parens ((string "+"    >> (Plus <$> (spaces1 >> ex) <*> (spaces1 >> ex)))
         <|> try (string "head" >> (Head <$> (spaces1 >> ex)))
         <|> try (string "tail" >> (Tail <$> (spaces1 >> ex)))
         <|> try (string "if"   >> (If   <$> (spaces1 >> ex) <*> (spaces1 >> ex) <*> (spaces1 >> ex)))
         <|> try (App <$> var <*> (spaces1 >> ex))
         <|> ex)

ty :: Monad m => Parser m Ty
ty = try (string "Nat" >> return NatTy)
  <|> do t <- brackets ty
         lift $ freshListTy t

funty :: Monad m => Parser m FunTy
funty = do t1 <- ty
           t2 <- (spaces1 >> string "->" >> spaces1 >> ty)
           lift $ freshFunTy t1 t2

fun :: Monad m => Parser m (Var, Fun)
fun = spaces >>
      parens (string "fn" >>
              (,) <$> (spaces1 >> var)
                  <*> (Fun <$> (spaces1 >> parens funty)
                           <*> (spaces1 >> parens var)
                           <*> (spaces1 >> ex))) <* spaces

prog :: Monad m => Parser m Prog
prog = many1 fun

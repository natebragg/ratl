{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Parser (
    preorder,
    prog,
) where

import Language.Ratl.Ast (
    Embeddable(..),
    List(..),
    Sym(..),
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

import Text.Parsec (Parsec, ParsecT, Stream(..), try, many, (<|>), (<?>))
import Text.Parsec.Pos (newPos, sourceName, sourceLine, sourceColumn)
import Text.Parsec.Prim (
    tokenPrim,
    updateParserState,
    getParserState,
    setParserState,
    State(..),
    unexpected,
    )

reservedSyms = map embed [
    S "->",
    S "int", S "bool", S "unit", S "sym", S "list",
    S "define", S "if", S "let",
    S "#t", S "#f"
    ]

newtype Iterator = Iterator { runIterator :: Maybe ((Val, Iterator), Iterator) }

skipNext :: Iterator -> Iterator
skipNext = Iterator . fmap (\((v, _), skip) -> ((v, skip), skip)) . runIterator

preorder :: Val -> Iterator
preorder = gov (Iterator Nothing)
    where gov :: Iterator -> Val -> Iterator
          gov next v@(List vs) = Iterator $ Just ((v, gol next vs), next)
          gov next v           = Iterator $ Just ((v, next), next)
          gol :: Iterator -> List -> Iterator
          gol next (Cons v vs) = gov (gol next vs) v
          gol next Nil         = next

instance Monad m => Stream Iterator m Val where
    uncons = return . fmap fst . runIterator

type SexpParser = Parsec Iterator ()

satisfy :: (Show a, Stream s m a) => (a -> Bool) -> ParsecT s u m a
satisfy f = tokenPrim (\a -> show a)
                      (\pos _a _as -> newPos (sourceName pos) (sourceLine pos) (1 + sourceColumn pos))
                      (\a -> if f a then Just a else Nothing)

item :: (Show a, Eq a, Stream s m a) => a -> ParsecT s u m a
item a = satisfy (==a) <?> show a

anyItem :: (Show a, Stream s m a) => ParsecT s u m a
anyItem = satisfy (const True)

consume :: SexpParser Val
consume = updateParserState (\st -> st {stateInput = skipNext $ stateInput st}) >> anyItem

int :: SexpParser Val
int = satisfy (\case Nat _ -> True; _ -> False)

boolean :: SexpParser Val
boolean = satisfy (\case Boolean _ -> True; _ -> False)

sym :: SexpParser Val
sym = satisfy (\case Sym _ -> True; _ -> False)

list :: SexpParser a -> SexpParser a
list p = do
    st <- getParserState
    ((v, _), skip) <- maybe (unexpected "end of expression") return $
                      runIterator $ stateInput st
    setParserState $ st {stateInput = preorder v}
    satisfy (\case List _ -> True; _ -> False)
    result <- p
    updateParserState $ \st -> st {stateInput = Iterator $ Just ((v, skip), skip)}
    anyItem
    return result

identifier :: SexpParser Val
identifier = try $ do
    name <- sym
    if project name `elem` reservedSyms
    then unexpected $ "reserved name " ++ show name
    else return name

reserved :: Sym -> SexpParser ()
reserved s = try $ item (Sym s) >> return ()

var :: SexpParser Var
var = (\(Sym (S x)) -> V x) <$> identifier
  <?> "identifier"

val :: SexpParser Val
val = int
  <|> boolean
  <|> try (list $ reserved (S "quote") >> consume)
  <?> "value"

ex :: SexpParser Ex
ex = Var <$> var
 <|> Val <$> val
 <|> (list $ (reserved (S "if")  >> If <$> ex <*> ex <*> ex)
         <|> (reserved (S "let") >> Let <$> (list $ many $ list $ (,) <$> var <*> ex) <*> ex)
         <|> (App <$> var <*> many ex))

ty :: SexpParser Ty
ty = (reserved (S "int") >> return NatTy)
 <|> (reserved (S "bool") >> return BooleanTy)
 <|> (reserved (S "unit") >> return UnitTy)
 <|> (reserved (S "sym") >> return SymTy)
 <|> (list $ reserved (S "list") >> ListTy <$> ty)
 <?> "type"

funty :: SexpParser FunTy
funty = do
    t1 <- ty
    reserved (S "->")
    t2 <- ty
    return $ Arrow t1 t2

fun :: SexpParser (Var, Fun)
fun = (list $ reserved (S "define") >>
              (,) <$> var
                  <*> (Fun <$> list funty
                           <*> list var
                           <*> ex))
  <?> "function definition"

prog :: SexpParser Prog
prog = makeProg <$> (list $ many fun)

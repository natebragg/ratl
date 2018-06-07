{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Parser (
    preorder,
    prog,
) where

import Language.Ratl.Val (
    Span(..),
    Embeddable(..),
    Val,
    LitList(..),
    Lit(..),
    litSpan,
    )
import Language.Ratl.Ast (
    Var(..),
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
import Text.Parsec.Prim (
    tokenPrim,
    updateParserState,
    getParserState,
    setParserState,
    State(..),
    unexpected,
    )

reservedSyms = [
    "->",
    "int", "bool", "unit", "sym", "list",
    "define", "if", "let",
    "#t", "#f"
    ]

newtype Iterator = Iterator { runIterator :: Maybe ((Lit, Iterator), Iterator) }

skipNext :: Iterator -> Iterator
skipNext = Iterator . fmap (\((v, _), skip) -> ((v, skip), skip)) . runIterator

preorder :: Lit -> Iterator
preorder = gov (Iterator Nothing)
    where gov :: Iterator -> Lit -> Iterator
          gov next v@(LitList _ vs) = Iterator $ Just ((v, gol next vs), next)
          gov next v                = Iterator $ Just ((v, next), next)
          gol :: Iterator -> LitList -> Iterator
          gol next (LitCons v vs) = gov (gol next vs) v
          gol next LitNil         = next

instance Monad m => Stream Iterator m Lit where
    uncons = return . fmap fst . runIterator

type SexpParser = Parsec Iterator ()

satisfy :: Monad m => (Lit -> Bool) -> ParsecT Iterator u m Lit
satisfy f = tokenPrim (\a -> show a)
                      (\pos t i -> case (litSpan t, fmap (litSpan . fst . fst) $ runIterator i) of
                        (_, Just (Span start _)) -> start
                        (Span _ stop, _) -> stop
                        otherwise -> pos)
                      (\a -> if f a then Just a else Nothing)

item :: Monad m => Lit -> ParsecT Iterator u m Lit
item a = satisfy (==a) <?> show a

anyItem :: Monad m => ParsecT Iterator u m Lit
anyItem = satisfy (const True)

consume :: SexpParser Lit
consume = updateParserState (\st -> st {stateInput = skipNext $ stateInput st}) >> anyItem

int :: SexpParser Lit
int = satisfy (\case LitNat _ _ -> True; _ -> False)

boolean :: SexpParser Lit
boolean = satisfy (\case LitBoolean _ _ -> True; _ -> False)

sym :: SexpParser Lit
sym = satisfy (\case LitSym _ _ -> True; _ -> False)

list :: SexpParser a -> SexpParser a
list p = do
    st <- getParserState
    ((v, _), skip) <- maybe (unexpected "end of expression") return $
                      runIterator $ stateInput st
    setParserState $ st {stateInput = preorder v}
    satisfy (\case LitList _ _ -> True; _ -> False)
    result <- p
    updateParserState $ \st -> st {stateInput = Iterator $ Just ((v, skip), skip)}
    anyItem
    return result

identifier :: SexpParser Lit
identifier = try $ do
    name@(LitSym _ s) <- sym
    if s `elem` reservedSyms
    then unexpected $ "reserved name " ++ show name
    else return name

reserved :: String -> SexpParser ()
reserved s = try $ item (LitSym Unknown s) >> return ()

var :: SexpParser Var
var = (\(LitSym _ x) -> V x) <$> identifier
  <?> "identifier"

val :: SexpParser Val
val = embed <$> int
  <|> embed <$> boolean
  <|> try (embed <$> (list $ reserved "quote" >> consume))
  <?> "value"

ex :: SexpParser Ex
ex = Var <$> var
 <|> Val <$> val
 <|> (list $ (reserved "if"  >> If <$> ex <*> ex <*> ex)
         <|> (reserved "let" >> Let <$> (list $ many $ list $ (,) <$> var <*> ex) <*> ex)
         <|> (App <$> var <*> many ex))

ty :: SexpParser Ty
ty = (reserved "int" >> return NatTy)
 <|> (reserved "bool" >> return BooleanTy)
 <|> (reserved "unit" >> return UnitTy)
 <|> (reserved "sym" >> return SymTy)
 <|> (list $ reserved "list" >> ListTy <$> ty)
 <?> "type"

funty :: SexpParser FunTy
funty = do
    t1 <- ty
    reserved "->"
    t2 <- ty
    return $ Arrow t1 t2

fun :: SexpParser (Var, Fun)
fun = (list $ reserved "define" >>
              (,) <$> var
                  <*> (Fun <$> list funty
                           <*> list var
                           <*> ex))
  <?> "function definition"

prog :: SexpParser Prog
prog = makeProg <$> (list $ many fun)

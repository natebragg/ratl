{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Parser (
    iterator,
    prog,
    ex,
    eol,
) where

import Language.Ratl.Val (
    Span(..),
    Embeddable(..),
    Val,
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
    Tyvar,
    Ty(..),
    FunTy(..),
    )

import Text.Parsec (
    Parsec,
    ParsecT,
    Stream(..),
    notFollowedBy,
    lookAhead,
    try,
    many,
    many1,
    (<|>),
    (<?>),
    )
import Text.Parsec.Prim (
    tokenPrim,
    updateParserState,
    getParserState,
    setParserState,
    State(..),
    unexpected,
    )

reservedTySyms = [
    "->",
    "int", "bool", "unit", "sym", "list", "pair"
    ]
reservedSyms = [
    "define", "if", "let",
    "#t", "#f"
    ]

newtype Iterator = Iterator { runIterator :: Maybe (Lit, Iterator) }

iterator :: Lit -> Iterator
iterator = go (Iterator Nothing)
    where go :: Iterator -> Lit -> Iterator
          go next (LitCons _ f s) = Iterator $ Just (f, go next s)
          go next v               = Iterator $ Just (v, next)

instance Monad m => Stream Iterator m Lit where
    uncons = return . runIterator

type SexpParser = Parsec Iterator ()

withStream :: Monad m => Iterator -> ParsecT Iterator u m a -> ParsecT Iterator u m a
withStream s' p = do
    s <- stateInput <$> getParserState
    updateParserState $ \st -> st {stateInput = s'}
    result <- p
    updateParserState $ \st -> st {stateInput = s}
    return result

satisfy :: Monad m => (Lit -> Bool) -> ParsecT Iterator u m Lit
satisfy f = tokenPrim (\a -> show a)
                      (\pos t i -> case (litSpan t, fmap (litSpan . fst) $ runIterator i) of
                        (_, Just (Span start _)) -> start
                        (Span _ stop, _) -> stop
                        otherwise -> pos)
                      (\a -> if f a then Just a else Nothing)

item :: Monad m => Lit -> ParsecT Iterator u m Lit
item a = satisfy (==a) <?> show a

anyItem :: Monad m => ParsecT Iterator u m Lit
anyItem = satisfy (const True)

int :: SexpParser Lit
int = satisfy (\case LitNat _ _ -> True; _ -> False)

boolean :: SexpParser Lit
boolean = satisfy (\case LitBoolean _ _ -> True; _ -> False)

sym :: SexpParser Lit
sym = satisfy (\case LitSym _ _ -> True; _ -> False)

nil :: SexpParser Lit
nil = satisfy (\case LitNil _ -> True; _ -> False)

cons :: SexpParser Lit
cons = satisfy (\case LitCons _ _ _ -> True; _ -> False)

eol :: SexpParser ()
eol = do
    -- This is subtle; A well-formed list ends with nil, with no further
    -- tokens.  Parsec fails uncontrollably after trying to read past the
    -- end of input.  So, to give good error messages, the parse must be
    -- circuitous, and "unexpected" here works around another nuance.
    try nil <|> do notFollowedBy anyItem
                   unexpected "malformed end of list; possible misuse of '.'"
    notFollowedBy anyItem
  <?> "end of list"

list :: SexpParser a -> SexpParser a
list p = do
    v <- cons
    withStream (iterator v) (p <* eol)
  <?> "list"

identifier :: SexpParser Lit
identifier = try $ do
    name@(LitSym _ s) <- sym
    if s `elem` reservedSyms
    then unexpected $ "reserved name " ++ show name
    else return name

reserved :: String -> SexpParser ()
reserved s = try $ item (LitSym Unknown s) >> return ()

quoted :: SexpParser Lit
quoted = try $ list $ reserved "quote" >> anyItem

var :: SexpParser Var
var = (\(LitSym _ x) -> V x) <$> identifier
  <?> "identifier"

val :: SexpParser Val
val = embed <$> int
  <|> embed <$> boolean
  <|> embed <$> quoted
  <?> "value"

ex :: SexpParser Ex
ex = Var <$> var
 <|> Val <$> val
 <|> (list $ (reserved "if"  >> If <$> ex <*> ex <*> ex)
         <|> (reserved "let" >> Let <$> (list $ many $ list $ (,) <$> var <*> ex) <*> ex)
         <|> (App <$> var <*> many ex))

tyvar :: SexpParser Tyvar
tyvar = do
    LitSym _ s <- quoted
    return s
 <?> "tyvar"

ty :: SexpParser Ty
ty = (reserved "int" >> return NatTy)
 <|> (reserved "bool" >> return BooleanTy)
 <|> (reserved "unit" >> return UnitTy)
 <|> (reserved "sym" >> return SymTy)
 <|> (Tyvar <$> tyvar)
 <|> (list $ (reserved "list" >> ListTy <$> ty)
         <|> (reserved "pair" >> PairTy <$> ty <*> ty))
 <?> "type"

funty :: SexpParser FunTy
funty = do
    t1 <- many1 ty
    reserved "->"
    t2 <- ty
    return $ Arrow t1 t2

fun :: SexpParser (Var, Fun)
fun = (list $ reserved "define" >>
              (,) <$> var
                  <*> (Fun <$> list funty
                           <*> list (many1 var)
                           <*> ex))
  <?> "function definition"

prog :: SexpParser Prog
prog = makeProg <$> (many fun)

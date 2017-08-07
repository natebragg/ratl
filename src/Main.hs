module Main where

import Data.Clp.Clp
import Data.Maybe (fromJust)
import Control.Arrow (second)

data Ty = NatTy | ListTy Ty | MysteryTy
    deriving (Eq)

instance Show Ty where
    show NatTy = "Nat"
    show (ListTy t) = "[" ++ show t ++ "]"
    show MysteryTy = "???"

data FunTy = Arrow Ty Ty
    deriving (Eq)

instance Show FunTy where
    show (Arrow t t') = show t ++ " -> " ++ show t'

data Nat = Z | S Nat
    deriving (Eq)

instance Show Nat where
    show n = show $ fromNat n

data List = Nil | Cons Val List
    deriving (Show, Eq)

data Var = V String
    deriving (Show, Eq)

data Val = List List | Nat Nat
    deriving (Show, Eq)

data Fun = Fun FunTy Var Ex
    deriving (Show, Eq)

data Ex = Plus Ex Ex | Head Ex | Tail Ex | Var Var | Val Val | App Var Ex | If Ex Ex Ex
    deriving (Show, Eq)

type Prog = [(Var, Fun)]

plus :: Nat -> Nat -> Nat
plus Z n      = n
plus n Z      = n
plus (S n) n' = plus n (S n')

fromNat :: Nat -> Int
fromNat Z = 0
fromNat (S n) = 1 + fromNat n

toNat :: Int -> Nat
toNat x | x <= 0 = Z
toNat x = S $ toNat $ x - 1

embed :: [Int] -> List
embed []     = Nil
embed (x:xs) = Cons (Nat $ toNat x) (embed xs)

run :: Prog -> [Int] -> Val
run phi args = eval [] (App (V "main") (Val $ List $ embed args))
    where eval rho (Plus e1 e2)  = let Nat n1 = eval rho e1 in let Nat n2 = eval rho e2 in Nat $ plus n1 n2
          eval rho (Head e)      = let List (Cons x  _) = eval rho e in x
          eval rho (Tail e)      = let List (Cons _ xs) = eval rho e in List xs
          eval rho (Var x)       = fromJust $ lookup x rho
          eval rho (Val v)       = v
          eval rho (App f e)     = let (Fun _ x b) = fromJust (lookup f phi) in eval ((x, eval rho e):rho) b
          eval rho (If ep et ef) =
                case eval rho ep of
                    (List Nil) -> eval rho ef
                    (Nat Z)    -> eval rho ef
                    _          -> eval rho et

check :: Prog -> Bool
check fs = all (elabF . snd) fs
    where sigma = map (second tyOf) fs
          tyOf (Fun ty _ _) = ty
          elabF (Fun (Arrow ty ty') x e) = Just ty' == elabE [(x, ty)] e
          elabE :: [(Var, Ty)] -> Ex -> Maybe Ty
          elabE gamma e = elab e
             where elab (Plus e1 e2)  = case (elab e1, elab e2) of (Just NatTy, Just NatTy) -> Just NatTy; _ -> Nothing
                   elab (Head e)      = case elab e of Just (ListTy ty) -> Just ty; _ -> Nothing
                   elab (Tail e)      = case elab e of ty@(Just (ListTy _ )) -> ty; _ -> Nothing
                   elab (Var x)       = lookup x gamma
                   elab (Val v)       = elabV v
                   elab (App f e)     = case (elab e, lookup f sigma) of (Just ty, Just (Arrow ty' ty'')) | ty == ty' -> Just ty''; _ -> Nothing
                   elab (If ep et ef) = case (elab ep, elab et, elab ef) of (Just _, tyt, tyf) | tyt == tyf -> tyt; _ -> Nothing
          elabV (Nat _)  = Just NatTy
          elabV (List l) = elabL l
          elabL Nil        = Just $ ListTy MysteryTy
          elabL (Cons v l) = if lty == (Just $ ListTy MysteryTy) then vty else if lty == vty then lty else Nothing
              where vty = fmap ListTy $ elabV v
                    lty = elabL l

main :: IO ()
main = do
  putStrLn $ "Using Clp version " ++ version ++ ": "
                                  ++ show (versionMajor, versionMinor, versionRelease)
  do
        let sum_ty = Arrow (ListTy NatTy) NatTy
        let main_ty = Arrow (ListTy NatTy) NatTy
        let id_list_ty = Arrow (ListTy NatTy) (ListTy NatTy)
        let p = [((V "sum"), Fun sum_ty (V "vals") $
                    If (Var $ V "vals")
                        (Plus (Head $ Var $ V "vals")
                              (App (V "sum")
                                   (Tail $ Var $ V "vals")))
                        (Val $ Nat Z)),
                ((V "id_list"), Fun id_list_ty (V "args") $
                    Var $ V "args"),
                ((V "main"), Fun main_ty (V "args") $
                    App (V "sum") (Var $ V "args"))]
        let checked = check p
        case checked of
         False ->
            putStrLn "Typechecking failed"
         True ->
            print $ run p [1..10]

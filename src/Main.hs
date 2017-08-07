module Main where

import Data.Maybe (fromJust, isJust)
import Control.Applicative (empty)
import Control.Monad (when, guard)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Arrow (second)
import System.Exit (exitFailure)

import Data.Clp.Clp
import Data.Clp.StandardForm (StandardForm(..), solve)

type Anno = Int

data Ty = NatTy | ListTy Anno Ty | MysteryTy

instance Show Ty where
    show NatTy = "Nat"
    show (ListTy _ t) = "[" ++ show t ++ "]"
    show MysteryTy = "forall a. a"

isListTy :: Ty -> Bool
isListTy NatTy = False
isListTy     _ = True

isNatTy :: Ty -> Bool
isNatTy (ListTy _ _) = False
isNatTy            _ = True

eqTy :: Ty -> Ty -> Bool
eqTy MysteryTy _ = True
eqTy _ MysteryTy = True
eqTy NatTy NatTy = True
eqTy (ListTy _ t) (ListTy _ t') = eqTy t t'
eqTy     _     _ = False

data FunTy = Arrow Anno Ty Ty

eqFun :: FunTy -> FunTy -> Bool
eqFun (Arrow _ t t'') (Arrow _ t' t''') = eqTy t t' && eqTy t'' t'''

instance Show FunTy where
    show (Arrow _ t t') = show t ++ " -> " ++ show t'

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
    deriving (Show)

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

type Cost = Double

k_plus, k_head, k_tail, k_var, k_val, k_app, k_ifp, k_ift, k_iff :: Cost
k_plus = 1.0
k_head = 1.0
k_tail = 1.0
k_var = 1.0
k_val = 1.0
k_app = 2.0
k_ifp = 1.0
k_ift = 1.0
k_iff = 1.0

type Constraint = ([(Anno, Double)], Cost)

freshAnno :: Monad m => StateT Anno m Anno
freshAnno = do
    q <- get
    put (q + 1)
    return q

freshListTy :: Monad m => Ty -> StateT Anno m Ty
freshListTy tau = do
    p <- freshAnno
    return $ ListTy p tau

freshFunTy :: Monad m => Ty -> Ty -> StateT Anno m FunTy
freshFunTy tau tau' = do
    q <- freshAnno
    return $ Arrow q tau tau'

type FunEnv = [(Var, FunTy)]

check :: Monad m => Prog -> StateT Anno (MaybeT m) (FunEnv, [Constraint])
check fs = (,) sigma <$> concat <$> mapM (elabF . snd) fs
    where sigma = map (second tyOf) fs
          tyOf (Fun ty _ _) = ty
          elabF :: Monad m => Fun -> StateT Anno (MaybeT m) [Constraint]
          elabF (Fun (Arrow qf ty ty') x e) = do
                    (ty'', q, cs) <- elabE [(x, ty)] e
                    guard $ eqTy ty' ty''
                    return $ ([(q, 1.0), (qf, -1.0)], 0.0):cs
          elabE :: Monad m => [(Var, Ty)] -> Ex -> StateT Anno (MaybeT m) (Ty, Anno, [Constraint])
          elabE gamma e = elab e
             where elab :: Monad m => Ex -> StateT Anno (MaybeT m) (Ty, Anno, [Constraint])
                   elab (Plus e1 e2)  = do (ty1, q1, cs1) <- elab e1
                                           (ty2, q2, cs2) <- elab e2
                                           q <- freshAnno
                                           guard $ isNatTy ty1 && isNatTy ty2
                                           return (NatTy, q, ([(q, 1.0), (q1, -1.0), (q2, -1.0)], k_plus):cs1 ++ cs2)
                   elab (Head e)      = do (ty, qe, cs) <- elab e
                                           guard $ isListTy ty
                                           let ListTy p ty' = ty
                                           q <- freshAnno
                                           return (ty', q, ([(q, 1.0), (p, 1.0), (qe, -1.0)], k_head):cs)
                   elab (Tail e)      = do (ty, qe, cs) <- elab e
                                           guard $ isListTy ty
                                           let ListTy p _ = ty
                                           q <- freshAnno
                                           return (ty, q, ([(q, 1.0), (p, 1.0), (qe, -1.0)], k_tail):cs)
                   elab (Var x)       = do let bnd = lookup x gamma
                                           guard $ isJust bnd
                                           let Just ty = bnd
                                           q <- freshAnno
                                           return (ty, q, [([(q, 1.0)], k_var)])
                   elab (Val v)       = do ty <- elabV v
                                           q <- freshAnno
                                           return (ty, q, [([(q, 1.0)], k_val)])
                   elab (App f e)     = do (ty, qe, cs) <- elab e
                                           q <- freshAnno
                                           let sig = lookup f sigma
                                           guard $ isJust sig
                                           let Just (Arrow qf ty' ty'') = sig
                                           guard $ eqTy ty ty'
                                           let equiv = case (ty, ty') of
                                                        (ListTy p _, ListTy pf _) | p /= pf -> [([(p, 1.0), (pf, -1.0)], 0.0)]
                                                        _                                   -> []
                                           return (ty'', q, ([(q, 1.0), (qe, -1.0), (qf, -1.0)], k_app):equiv ++ cs)
                   elab (If ep et ef) = do (typ, qp, csp) <- elab ep
                                           (tyt, qt, cst) <- elab et
                                           (tyf, qf, csf) <- elab ef
                                           q <- freshAnno
                                           guard $ eqTy tyt tyf
                                           return (tyt, q, ([(q, 1.0), (qp, -1.0), (qt, -1.0)], k_ifp + k_ift):
                                                           ([(q, 1.0), (qp, -1.0), (qf, -1.0)], k_ifp + k_iff):csp ++ cst ++ csf)
          elabV :: Monad m => Val -> StateT Anno (MaybeT m) Ty
          elabV (Nat _)  = return NatTy
          elabV (List l) = elabL l
          elabL :: Monad m => List -> StateT Anno (MaybeT m) Ty
          elabL Nil        = freshListTy MysteryTy
          elabL (Cons v l) = do
                vty <- elabV v
                lty <- elabL l
                case lty of
                    ListTy p MysteryTy      -> return $ ListTy p vty
                    ListTy _ l | eqTy l vty -> return lty
                    _                       -> empty

xlate :: Constraint -> ([Double], Cost)
xlate (cs, c) = (xl 0 (replicate (1 + (maximum $ map fst cs)) 0.0) cs, c)
    where xl _      []           _ = []
          xl n (r:row)          [] = r:xl (n + 1) row cs
          xl n (r:row) ((q, v):cs) = xl n ((if q == n then r + v else r):row) cs

objective :: FunEnv -> [Double]
objective sigma = fst $ xlate $ (obj sigma, 0.0)
    where obj [] = []
          obj ((_, Arrow q (ListTy p _) _):sigma) = (q, 1.0):(p, 1000):obj sigma
          obj ((_, Arrow q            _ _):sigma) = (q, 1.0):obj sigma

interpret :: [Double] -> FunTy -> String
interpret optimum (Arrow q (ListTy p _) _) = (show $ optimum !! p) ++ "*n + " ++ (show $ optimum !! q)
interpret optimum (Arrow q            _ _) = show $ optimum !! q

main :: IO ()
main = do
    putStrLn $ "Using Clp version " ++ version ++ ": "
                                    ++ show (versionMajor, versionMinor, versionRelease)
    result <- runMaybeT $ flip evalStateT 0 $ do
        sum_ty  <- freshListTy NatTy >>= flip freshFunTy NatTy
        main_ty <- freshListTy NatTy >>= flip freshFunTy NatTy
        id_list_ty <- freshListTy NatTy >>= \ty -> freshFunTy ty ty
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
        checked <- check p
        return (checked, p)
    case result of
        Nothing ->
            putStrLn "Typechecking failed"
        Just ((env, constraints), p) -> do
            print env
            let optimum = solve $ StandardForm (objective env, map xlate constraints)
            when (null optimum) $ do
                putStrLn "Analysis was infeasible"
                exitFailure
            let complexities = map (second $ interpret optimum) env
            mapM_ (\(V f, complexity) -> putStrLn $ f ++ ": " ++ complexity) complexities
            print $ run p [1..10]

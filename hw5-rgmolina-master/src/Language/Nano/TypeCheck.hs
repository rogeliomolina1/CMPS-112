{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}

module Language.Nano.TypeCheck where

import Language.Nano.Types
import Language.Nano.Parser

import qualified Data.List as L
import           Text.Printf (printf)
import           Control.Exception (throw)

--------------------------------------------------------------------------------
typeOfFile :: FilePath -> IO Type
typeOfFile f = readFile f >>= typeOfString

typeOfString :: String -> IO Type
typeOfString s = typeOfExpr (parseExpr s)

typeOfExpr :: Expr -> IO Type
typeOfExpr e = do
  let (!st, t) = infer initInferState preludeTypes e
  if (length (stSub st)) < 0 then throw (Error ("count Negative: " ++ show (stCnt st)))
  else return t

--------------------------------------------------------------------------------
-- Problem 1: Warm-up
--------------------------------------------------------------------------------

-- | Things that have free type variables
class HasTVars a where
  freeTVars :: a -> [TVar]

-- | Type variables of a type
instance HasTVars Type where
  freeTVars TInt        =  []
  freeTVars TBool       =  []
  freeTVars (t1 :=> t2) = L.nub ((freeTVars t1) ++ (freeTVars t2))
  freeTVars (TVar t)    = [t]
  freeTVars (TList t)   = L.nub (freeTVars t)

-- | Free type variables of a poly-type (remove forall-bound vars)
instance HasTVars Poly where
  freeTVars (Mono t)        = freeTVars t
  freeTVars (Forall s (t))  = L.delete s (L.nub (freeTVars t))

-- | Free type variables of a type environment
instance HasTVars TypeEnv where
  freeTVars gamma   = concat [freeTVars s | (x, s) <- gamma]

-- | Lookup a variable in the type environment
lookupVarType :: Id -> TypeEnv -> Poly
lookupVarType x ((y, s) : gamma)
  | x == y    = s
  | otherwise = lookupVarType x gamma
lookupVarType x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend the type environment with a new biding
extendTypeEnv :: Id -> Poly -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma

-- | Lookup a type variable in a substitution;
--   if not present, return the variable unchanged
lookupTVar :: TVar -> Subst -> Type
lookupTVar a sub
  | sub == []            = (TVar a)
  | a == fst(head sub) = snd(head sub)
  | (tail sub) /= []   = lookupTVar a (tail sub)
  | otherwise = (TVar a)

-- | Remove a type variable from a substitution
removeTVar :: TVar -> Subst -> Subst
removeTVar a sub
  | sub == []            = sub
  | a == fst(head sub) = (L.delete (head sub) sub)
  | (tail sub) /= []   = removeTVar a (tail sub)
  | otherwise = sub

-- | Things to which type substitutions can be apply
class Substitutable a where
  apply :: Subst -> a -> a

-- | Apply substitution to type
instance Substitutable Type where
  apply sub TInt        = TInt
  apply sub TBool       = TBool
  apply sub (t1 :=> t2) = (apply sub t1) :=> (apply sub t2)
  apply sub (TVar t)    = lookupTVar t sub
  apply sub (TList t)   = TList (apply sub t)

-- | Apply substitution to poly-type
instance Substitutable Poly where
  apply sub (Mono t)       = Mono (apply sub t)
  apply sub (Forall s (t)) = Forall s (apply (removeTVar s sub) t)

-- | Apply substitution to (all poly-types in) another substitution
instance Substitutable Subst where
  apply sub to = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip to

-- | Apply substitution to a type environment
instance Substitutable TypeEnv where
  apply sub gamma = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip gamma

-- | Extend substitution with a new type assignment
extendSubst :: Subst -> TVar -> Type -> Subst
extendSubst sub a t = [(a, apply sub t)] ++ (apply [(a, apply sub t)] sub)

--------------------------------------------------------------------------------
-- Problem 2: Unification
--------------------------------------------------------------------------------

-- | State of the type inference algorithm
data InferState = InferState {
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
} deriving Show

-- | Initial state: empty substitution; 0 type variables
initInferState = InferState [] 0

-- | Fresh type variable number n
freshTV n = TVar $ "a" ++ show n

-- | Extend the current substitution of a state with a new type assignment
extendState :: InferState -> TVar -> Type -> InferState
extendState (InferState sub n) a t = InferState (extendSubst sub a t) n

-- | Unify a type variable with a type;
--   if successful return an updated state, otherwise throw an error
unifyTVar :: InferState -> TVar -> Type -> InferState
unifyTVar st a t
  | TVar a == t = st
  | elem a (freeTVars t) = throw (Error ("type error: cannot unify " ++ a ++ " and " ++ (show t) ++ " (occurs check)"))
  | otherwise = (extendState st a t)

-- | Unify two types;
--   if successful return an updated state, otherwise throw an error
unify :: InferState -> Type -> Type -> InferState
unify st t1 (TVar t)             = unifyTVar st t t1
unify st (TVar t) t2             = unifyTVar st t t2

unify st TInt TInt               = st

unify st TBool TBool             = st

unify st (t1 :=> t2) (t3 :=> t4) = unify (unify st t1 t3) t2 t4

unify st (TList t1) (TList t2)   = unify st t1 t2

unify st t1 t2                   = throw (Error ("type error: cannot unify " ++ (show t1) ++ " and " ++ (show t2)))

--------------------------------------------------------------------------------
-- Problem 3: Type Inference
--------------------------------------------------------------------------------

infer :: InferState -> TypeEnv -> Expr -> (InferState, Type)
infer st _   (EInt _)          = (st, TInt)
infer st _   (EBool _)         = (st, TBool)
-- Helped by Lucas in MSI
infer st gamma (EVar x)        = (st1, t)
  where
    (i, t) = instantiate (stCnt st) (lookupVarType x gamma)
    st1    = InferState (stSub st) i
-------------------------
--From Slides
infer st gamma (ELam x body)   = (st1,  tX'  :=>  tBody)
  where
        tEnv'                   =  extendTypeEnv  x  (Mono tX)  gamma
        tX                     =  freshTV (stCnt st)
        (st1,  tBody)           =  infer  st  tEnv'  body
        tX'                     =  apply  (stSub st1)  tX
-------------------------
---Based on Lucas' notes from MSI
infer st gamma (EApp e1 e2)    =  (st3, t3)
  where
    (st1, t1) = infer st gamma e1
    gamma1    = apply (stSub st1) gamma
    (st2, t2) = infer st1 gamma1 e2
    tv        = freshTV ((stCnt st2)+1)
    st3       = unify st2 t1 (t2 :=> tv)
    t3        = apply (stSub st3) tv
----------------
--Office hours with Ana
infer st gamma (ELet x e1 e2)  = infer st1 gamma2 e2
  where
    (st1, t1) = infer st gamma e1
    gamma1    = apply (stSub st1) gamma
    s1        = generalize gamma1 t1
    gamma2    = extendTypeEnv x s1 gamma1
-------------------------------------
infer st gamma (EBin op e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp opVar e1) e2
    opVar = EVar (show op)
infer st gamma (EIf c e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp (EApp ifVar c) e1) e2
    ifVar = EVar "if"
infer st gamma ENil = infer st gamma (EVar "[]")

-- | Generalize type variables inside a type
generalize :: TypeEnv -> Type -> Poly
-- Lucas Simon & Kenji Mah helped me during this bit
generalize gamma t = foldl f base (reverse (L.nub ((freeTVars t) L.\\ (freeTVars gamma))))
  where
    f a x = Forall x (a)
    base  = (Mono t)
---------------------------------------------
-- | Instantiate a polymorphic type into a mono-type with fresh type variables
instantiate :: Int -> Poly -> (Int, Type)
instantiate n (Mono t)        = (n, t)
instantiate n (Forall s t)    = (instantiate (n + 1) (apply [(s, freshTV n)] t))

-- | Types of built-in operators and functions
preludeTypes :: TypeEnv
preludeTypes =
  [ ("+",    Mono $ TInt :=> TInt :=> TInt)
  , ("-",    Mono $ TInt :=> TInt :=> TInt)
  , ("*",    Mono $ TInt :=> TInt :=> TInt)
  , ("/",    Mono $ TInt :=> TInt :=> TInt)
  , ("==",   Forall "a" $ Mono $ TVar "a" :=> TVar "a" :=> TBool)
  , ("!=",   Forall "a" $ Mono $ TVar "a" :=> TVar"a" :=> TBool)
  , ("<",    Mono $ TInt :=> TInt :=> TBool)
  , ("<=",   Mono $ TInt :=> TInt :=> TBool)
  , ("&&",   Mono $ TBool :=> TBool :=> TBool)
  , ("||",   Mono $ TBool :=> TBool :=> TBool)
  ----Office hours with Ana
  , ("if",   Forall "a" . Mono $ TBool :=> "a" :=> "a" :=> "a")
  -- lists:
  , ("[]",   Forall "a" $ Mono $ TList "a")
  , (":",    Forall "a" $ Mono $ TVar "a" :=> TList (TVar "a") :=> TList (TVar "a"))
  , ("head", Forall "a" $ Mono $ TList (TVar "a") :=> TVar "a")
  , ("tail", Forall "a" $ Mono $ TList (TVar "a") :=> TList (TVar"a"))
  ]
  -------------

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Typing.Infer where

import Control.Monad
import Control.Monad.State
import Control.Placeholder (todo)
import qualified Data.Set as Set
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Debug.Trace (trace)
import qualified NIR as N
import Spanned
import Text.Pretty.Simple (pShow)
import qualified Typing.Constraint as Constraint
import qualified Typing.Context as Ctx
import qualified Typing.Scheme as Scheme
import qualified Typing.Solver as Solver
import Typing.TIR
import qualified Typing.Ty as Ty
import Typing.Types

data InferError = UnificationError Ty Ty | Occurs Ty Ty deriving (Show, Eq)

data Program
  = PFile Text (Spanned Module)
  deriving (Show)

applySubstProgram :: Subst -> Spanned Program -> Spanned Program
applySubstProgram s (Spanned (PFile n m) sp) = Spanned (PFile n (applySubstModule s m)) sp

data Module = Module Name [Typed Decl] deriving (Show)

applySubstModule :: Subst -> Spanned Module -> Spanned Module
applySubstModule s (Spanned (Module n ds) sp) = Spanned (Module n (fmap (applySubstDecl s) ds)) sp

data Decl
  = DDef (Typed Pattern) (Typed Expr)
  | DFn Name [Typed Pattern] (Typed Expr)
  | DFnMatch Name [([Typed Pattern], Typed Expr)]
  | DRecordDef Name [TyVar] [(Name, Spanned Ty)]
  | DData Name [TyVar] [(Name, [Spanned Ty])]
  | DTypeSyn Name [TyVar] (Spanned Ty)
  | DImport Path
  deriving (Show, Eq)

applySubstDecl :: Subst -> Typed Decl -> Typed Decl
applySubstDecl s (Typed (Spanned (DDef p e) sp) t) =
  Typed (Spanned (DDef (applySubstPattern s p) (applySubstExpr s e)) sp) (Ty.applySubst s t)
applySubstDecl s (Typed (Spanned (DFn n ps e) sp) t) =
  Typed (Spanned (DFn n (fmap (applySubstPattern s) ps) (applySubstExpr s e)) sp) (Ty.applySubst s t)
applySubstDecl s (Typed (Spanned (DFnMatch n pes) sp) t) =
  Typed (Spanned (DFnMatch n (fmap (\(ps, e) -> (fmap (applySubstPattern s) ps, applySubstExpr s e)) pes)) sp) (Ty.applySubst s t)
-- applySubstDecl s (Typed (Spanned (DRecordDef n vs fs) sp) t) =
--   Typed (Spanned (DRecordDef n vs (fmap (\(n, t) -> (n, fmap (Ty.applySubst s) t)) fs)) sp) (Ty.applySubst s t)
applySubstDecl _ _ = todo

data Expr
  = ELit Lit
  | EVar Name
  | EApp (Typed Expr) (Typed Expr)
  | ELam (Typed Pattern) (Typed Expr)
  | ELet (Typed Pattern) (Typed Expr) (Typed Expr)
  | EFn Name [Typed Pattern] (Typed Expr) (Typed Expr)
  | EIf (Typed Expr) (Typed Expr) (Typed Expr)
  | EMatch (Typed Expr) [(Typed Pattern, Typed Expr)]
  | EList [Typed Expr]
  | EArray (Array Int (Typed Expr))
  | ETuple [Typed Expr]
  | ERecord (Maybe Name) [(Name, Typed Expr)]
  | EUnit
  deriving (Show, Eq)

applySubstExpr :: Subst -> Typed Expr -> Typed Expr
applySubstExpr s (Typed e@(Spanned (ELit _) _) t) = Typed e (Ty.applySubst s t)
applySubstExpr s (Typed e@(Spanned (EVar _) _) t) = Typed e (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EApp e1 e2) sp) t) =
  Typed (Spanned (EApp (applySubstExpr s e1) (applySubstExpr s e2)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (ELam p e) sp) t) =
  Typed (Spanned (ELam (applySubstPattern s p) (applySubstExpr s e)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (ELet p e1 e2) sp) t) =
  Typed (Spanned (ELet (applySubstPattern s p) (applySubstExpr s e1) (applySubstExpr s e2)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EFn n ps e1 e2) sp) t) =
  Typed (Spanned (EFn n (fmap (applySubstPattern s) ps) (applySubstExpr s e1) (applySubstExpr s e2)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EIf e1 e2 e3) sp) t) =
  Typed (Spanned (EIf (applySubstExpr s e1) (applySubstExpr s e2) (applySubstExpr s e3)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EMatch e ps) sp) t) =
  Typed (Spanned (EMatch (applySubstExpr s e) (fmap (\(p, e') -> (applySubstPattern s p, applySubstExpr s e')) ps)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EList es) sp) t) =
  Typed (Spanned (EList (fmap (applySubstExpr s) es)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EArray es) sp) t) =
  Typed (Spanned (EArray (fmap (applySubstExpr s) es)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (ETuple es) sp) t) =
  Typed (Spanned (ETuple (fmap (applySubstExpr s) es)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (ERecord n fs) sp) t) =
  Typed (Spanned (ERecord n (fmap (\(n', e) -> (n', applySubstExpr s e)) fs)) sp) (Ty.applySubst s t)
applySubstExpr _ e = e

data Pattern
  = PWildcard
  | PLit Lit
  | PVar Name
  | PPair (Typed Pattern) (Typed Pattern)
  | PList [Typed Pattern]
  | PUnit
  deriving (Show, Eq)

-- applySubstPattern :: Subst -> Typed Pattern -> Typed Pattern
-- applySubstPattern s (Typed p@(Spanned PWildcard _) t) = Typed p (Ty.applySubst s t)
-- applySubstPattern s (Typed p@(Spanned (PVar _) _) t) = Typed p (Ty.applySubst s t)
-- applySubstPattern s (Typed (Spanned (PPair p1 p2) sp) t) =
--   Typed (Spanned (PPair (applySubstPattern s p1) (applySubstPattern s p2)) sp) (Ty.applySubst s t)
-- applySubstPattern s (Typed (Spanned (PList ps) sp) t) =
--   Typed (Spanned (PList (fmap (applySubstPattern s) ps)) sp) (Ty.applySubst s t)
-- applySubstPattern _ p = p
applySubstPattern :: Subst -> Typed Pattern -> Typed Pattern
applySubstPattern s p =
  -- trace ("applySubstPattern " ++ show p) $
  case p of
    (Typed (Spanned PWildcard sp) t) -> Typed (Spanned PWildcard sp) (Ty.applySubst s t)
    (Typed (Spanned (PVar n) sp) t) -> Typed (Spanned (PVar n) sp) (Ty.applySubst s t)
    (Typed (Spanned (PPair p1 p2) sp) t) ->
      Typed (Spanned (PPair (applySubstPattern s p1) (applySubstPattern s p2)) sp) (Ty.applySubst s t)
    (Typed (Spanned (PList ps) sp) t) ->
      Typed (Spanned (PList (fmap (applySubstPattern s) ps)) sp) (Ty.applySubst s t)
    _ -> p

type Name = Spanned (Text, ResId)

type ResId = Unique

type Path = Spanned [Name]

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq)

data Solver = Solver
  { constraints :: [Constraint],
    tyVarCounter :: Unique,
    subst :: Subst,
    ctx :: Context,
    errors :: [InferError]
  }
  deriving (Show)

type InferState = State Solver

defaultSolver :: Solver
defaultSolver =
  let s =
        Solver
          { constraints = [],
            tyVarCounter = Id 0,
            subst = Map.empty,
            ctx = Context [],
            errors = []
          }
   in execState builtins s

builtins :: InferState ()
builtins = do
  let m =
        Map.empty
          & Map.insert (Id 0) (Scheme [] $ TArrow TInt TInt)
          & Map.insert (Id 1) (Scheme [] $ TArrow TBool TBool)
          & Map.insert (Id 2) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 3) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 4) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 5) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 6) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 7) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 10) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 11) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 12) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 13) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
  v1 <- freshVar $ NoLoc
  let m1 = Map.insert (Id 8) (Scheme [v1] $ TArrow (TVar v1) (TArrow (TVar v1) TBool)) m
  v2 <- freshVar $ NoLoc
  let m2 = Map.insert (Id 9) (Scheme [v2] $ TArrow (TVar v2) (TArrow (TVar v2) TBool)) m1
  v3 <- freshVar $ NoLoc
  let m3 = Map.insert (Id 14) (Scheme [v3] $ TArrow (TVar v3) (TArrow (TList (TVar v3)) (TList (TVar v3)))) m2
  v4 <- freshVar $ NoLoc
  v5 <- freshVar $ NoLoc
  let m4 =
        Map.insert
          (Id 15)
          (Scheme [v4, v5] $ TArrow (TVar v4) (TArrow (TArrow (TVar v4) (TVar v5)) (TVar v5)))
          m3
  s <- get
  case ctx s of
    Context [] -> put s {ctx = Context $ [m4]}
    _ -> error "builtins: context is not empty"

freshVar :: Span -> InferState (Spanned TyVar)
freshVar sp = do
  s@Solver {tyVarCounter = c@(Id v)} <- get
  put s {tyVarCounter = Id (v + 1)}
  pure $ Spanned (TyVar c) sp

pushConstraint :: Constraint -> InferState ()
pushConstraint c = modify' $ \s@Solver {constraints = cs} -> s {constraints = c : cs}

pushError :: InferError -> InferState ()
pushError e = modify' $ \s@Solver {errors = es} -> s {errors = e : es}

newtype Constraint = Eq Ty Ty deriving (Show, Eq)

applySubstConstraint :: Subst -> Constraint -> Constraint
applySubstConstraint s (Eq t1 t2) = Eq (Ty.applySubst s t1) (Ty.applySubst s t2)

data Ty
  = TInt
  | TBool
  | TChar
  | TString
  | TUnit
  | TVar (Spanned TyVar)
  | TArrow Ty Ty
  | TList Ty
  | TArray Ty
  | TTuple [Ty]
  | TRecord [(String, Ty)]
  | TCon String [Ty]
  deriving (Show, Eq, Ord)

freeVarsTy :: Ty -> Set (Spanned TyVar)
freeVarsTy (TVar v) = Set.singleton v
freeVarsTy (TArrow t1 t2) = freeVarsTy t1 `Set.union` freeVarsTy t2
freeVarsTy (TList t) = freeVarsTy t
freeVarsTy (TArray t) = freeVarsTy t
freeVarsTy (TTuple ts) = Set.unions $ fmap freeVarsTy ts
freeVarsTy _ = Set.empty

applySubstTy :: Subst -> Ty -> Ty
applySubstTy s ty@(TVar v) = Map.findWithDefault ty v s
applySubstTy s (TArrow t1 t2) = TArrow (applySubstTy s t1) (applySubstTy s t2)
applySubstTy s (TList t) = TList (applySubstTy s t)
applySubstTy s (TArray t) = TArray (applySubstTy s t)
applySubstTy s (TTuple ts) = TTuple (fmap (applySubstTy s) ts)
applySubstTy _ t = t

generalize :: Ty -> InferState Scheme
generalize t = do
  Solver {ctx = c} <- get
  pure $ Scheme (Set.toList (freeVarsTy t `Set.difference` freeVarsCtx c)) t

unify :: Ty -> Ty -> InferState ()
unify t1 t2 = do
  -- trace ("unify " ++ (unpack $ pretty t1) ++ " and " ++ (unpack $ pretty t2)) $ pure ()
  case (t1, t2) of
    (TInt, TInt) -> pure ()
    (TBool, TBool) -> pure ()
    (TChar, TChar) -> pure ()
    (TString, TString) -> pure ()
    (TUnit, TUnit) -> pure ()
    (TArrow tp tr, TArrow tp' tr') -> do
      unify tp tp'
      unify tr tr'
    (TVar v1, _) -> bind v1 t2
    (_, TVar v2) -> bind v2 t1
    (TList t, TList t') -> unify t t'
    (TArray t, TArray t') -> unify t t'
    (TTuple ts1, TTuple ts2) -> zipWithM_ unify ts1 ts2
    _ -> do
      Solver.pushError $ UnificationError t1 t2

bind :: Spanned TyVar -> Ty -> InferState ()
bind v t
  | t == TVar v = pure ()
  | Set.member v (freeVars t) = do
      Solver.pushError $ Occurs (TVar v) t
  | otherwise = do
      s <- get
      let su = subst s
      -- traceM ("bind " ++ (unpack $ pretty v) ++ " to " ++ (unpack $ pretty t) ++ " gives " ++ (show $ Map.insert v t su))
      put s {subst = Map.insert v t su}

instance Pretty Ty where
  pretty TInt = "Int"
  pretty TBool = "Bool"
  pretty TChar = "Char"
  pretty TString = "String"
  pretty TUnit = "Unit"
  pretty (TVar v) = pretty v
  pretty (TArrow t1 t2) = case t1 of
    TArrow _ _ -> pack $ "(" ++ unpack (pretty t1) ++ ") -> " ++ unpack (pretty t2)
    _ -> pack $ unpack (pretty t1) ++ " -> " ++ unpack (pretty t2)
  pretty (TList t) = pack $ "[" ++ unpack (pretty t) ++ "]"
  pretty (TArray t) = pack $ "#[" ++ unpack (pretty t) ++ "]"
  pretty (TTuple ts) = pack $ "(" ++ unwords (fmap show ts) ++ ")"
  pretty (TRecord fs) = pack $ "{" ++ unwords (fmap (\(n, t) -> n ++ ": " ++ show t) fs) ++ "}"
  pretty (TCon n ts) = pack $ n ++ " " ++ unwords (fmap show ts)

type Subst = Map (Spanned TyVar) Ty

newtype TyVar = TyVar Unique deriving (Show, Eq, Ord)

instance Pretty TyVar where
  pretty (TyVar (Id i)) = pack $ "t" ++ show i

data Typed a = Typed (Spanned a) Ty deriving (Show, Eq)

newtype Context = Context [Map Unique Scheme] deriving (Show)

freeVars :: Context -> Set (Spanned TyVar)
freeVars (Context fs) =
  fs
    & (<$>) (Map.elems)
    & concat
    & (<$>) freeVarsScheme
    & Set.unions

pop :: InferState ()
pop = do
  s@Solver {constraints = _, subst = _, ctx = Context fs, errors = _} <- get
  case fs of
    [] -> error "cannot pop empty context"
    _ : [] -> error "cannot pop top-level context"
    _ : fs' -> put s {ctx = Context fs'}

push :: InferState ()
push = do
  s@Solver {constraints = _, subst = _, ctx = Context fs, errors = _} <- get
  put s {ctx = Context $ Map.empty : fs}

define :: Unique -> Scheme -> InferState ()
define n scm = do
  s@Solver {ctx = Context fs} <- get
  case fs of
    [] -> do
      let f = Map.singleton n scm
      put s {ctx = Context [f]}
    (f : fs') -> put s {ctx = Context $ Map.insert n scm f : fs'}
  pure ()

lookup :: Unique -> InferState Scheme
lookup n = do
  Solver {ctx = Context fs} <- get
  case lookup' n fs of
    Just s -> pure s
    Nothing -> error $ "unbound variable" <> show n
  where
    lookup' _ [] = Nothing
    lookup' n' (m : ms) = case Map.lookup n m of
      Just s -> Just s
      Nothing -> lookup' n' ms

applySubstCtx :: Subst -> Context -> Context
applySubstCtx s (Context fs) = Context $ fmap (fmap (applySubstScheme s)) fs

data Scheme = Scheme [Spanned TyVar] Ty deriving (Show)

inst :: Scheme -> InferState Ty
inst (Scheme vars t) = do
  vars' <- forM vars $ \(Spanned (TyVar _) sp) -> TVar <$> freshVar sp
  let s = Map.fromList $ zip vars vars'
  pure $ applySubstTy s t

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Scheme vars t) = Scheme vars (Ty.applySubst s t)

freeVarsScheme :: Scheme -> Set (Spanned TyVar)
freeVarsScheme (Scheme vars t) = t & freeVarsTy & Set.filter (`notElem` vars)

builtins :: InferState ()
builtins = do
  let m =
        Map.empty
          & Map.insert (Id 0) (Scheme [] $ TArrow TInt TInt)
          & Map.insert (Id 1) (Scheme [] $ TArrow TBool TBool)
          & Map.insert (Id 2) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 3) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 4) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 5) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 6) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 7) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 10) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 11) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 12) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 13) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
  v1 <- freshVar $ NoLoc
  let m1 = Map.insert (Id 8) (Scheme [v1] $ TArrow (TVar v1) (TArrow (TVar v1) TBool)) m
  v2 <- freshVar $ NoLoc
  let m2 = Map.insert (Id 9) (Scheme [v2] $ TArrow (TVar v2) (TArrow (TVar v2) TBool)) m1
  v3 <- freshVar $ NoLoc
  let m3 = Map.insert (Id 14) (Scheme [v3] $ TArrow (TVar v3) (TArrow (TList (TVar v3)) (TList (TVar v3)))) m2
  v4 <- freshVar $ NoLoc
  v5 <- freshVar $ NoLoc
  let m4 =
        Map.insert
          (Id 15)
          (Scheme [v4, v5] $ TArrow (TVar v4) (TArrow (TArrow (TVar v4) (TVar v5)) (TVar v5)))
          m3
  s <- get
  case ctx s of
    Context [] -> put s {ctx = Context $ [m4]}
    _ -> error "builtins: context is not empty"

genConstraints :: Spanned N.Program -> InferState (Spanned Program)
genConstraints (Spanned (N.PFile n m) s) = do
  m' <- genModuleConstraints m
  pure $ Spanned (PFile n m') s

genModuleConstraints :: Spanned N.Module -> InferState (Spanned Module)
genModuleConstraints (Spanned (N.Module n ds) s) = do
  ds' <- forM ds genDeclConstraints
  pure $ Spanned (Module n ds') s

genDeclConstraints :: Spanned N.Decl -> InferState (Typed Decl)
genDeclConstraints (Spanned (N.DDef p e) s) = do
  e'@(Typed _ te) <- genExprConstraints e
  p'@(Typed _ tp) <- genPatternConstraints p te True
  Solver.pushConstraint $ Eq tp te
  pure $ Typed (Spanned (DDef p' e') s) te
genDeclConstraints (Spanned (N.DFn n ps e) s) = do
  v <- TVar <$> Solver.freshVar s
  vps <- forM ps $ \(Spanned _ sp) -> Solver.freshVar sp
  Ctx.define (snd (value n)) (Scheme vps v)
  Ctx.push
  ps' <- forM (zip ps vps) $ \ ~(p, pv) -> genPatternConstraints p (TVar pv) False
  e'@(Typed _ te) <- genExprConstraints e
  let ty = foldr (\pv t -> TArrow (TVar pv) t) te vps
  Solver.pushConstraint $ Eq v ty
  Ctx.pop
  pure $ Typed (Spanned (DFn n ps' e') s) ty
genDeclConstraints (Spanned (N.DFnMatch n t cs) s) = todo
genDeclConstraints _ = todo

genExprConstraints :: Spanned N.Expr -> InferState (Typed Expr)
genExprConstraints (Spanned (N.ELit (N.LInt i)) s) = pure $ Typed (Spanned (ELit (LInt i)) s) TInt
genExprConstraints (Spanned (N.ELit (N.LBool b)) s) = pure $ Typed (Spanned (ELit (LBool b)) s) TBool
genExprConstraints (Spanned (N.ELit (N.LString t)) s) = pure $ Typed (Spanned (ELit (LString t)) s) TString
genExprConstraints (Spanned (N.EVar n) s) = do
  p <- Ctx.lookup (snd (value n))
  v <- Scheme.inst p
  pure $ Typed (Spanned (EVar n) s) v
genExprConstraints (Spanned (N.EApp f arg) s) = do
  ~f'@(Typed _ tf) <- genExprConstraints f
  ~arg'@(Typed _ targ) <- genExprConstraints arg
  v <- TVar <$> Solver.freshVar s
  let ty = TArrow targ v
  Solver.pushConstraint $ Eq tf ty
  -- trace ("genExprConstraints: EApp " ++ (unpack . toStrict $ pShow t1) ++ "\n" ++ (unpack . toStrict $ pShow (TArrow t2 v))) $
  --   pure ()
  Solver.pushConstraint $ Eq tf ty
  pure $ Typed (Spanned (EApp f' arg') s) v
genExprConstraints (Spanned (N.ELam p@(Spanned _ sp) e) s) = do
  Ctx.push
  v <- TVar <$> Solver.freshVar sp
  p' <- genPatternConstraints p v False
  e'@(Typed _ te) <- genExprConstraints e
  Ctx.pop
  pure $ Typed (Spanned (ELam p' e') s) (TArrow v te)
genExprConstraints (Spanned (N.ELet p e1 e2) s) = do
  e1'@(Typed _ t1) <- genExprConstraints e1
  Ctx.push
  p'@(Typed _ tp) <- genPatternConstraints p t1 True
  e2'@(Typed _ t2) <- genExprConstraints e2
  Solver.pushConstraint $ Eq tp t1
  Ctx.pop
  pure $ Typed (Spanned (ELet p' e1' e2') s) t2
genExprConstraints (Spanned (N.EFn n ps e b) s) = do
  -- trace "genExprConstraints: EFn" $ pure ()
  v <- TVar <$> Solver.freshVar s
  -- trace ("var: " ++ (unpack . toStrict $ pShow v)) $ pure ()
  vps <- forM ps $ \(Spanned _ sp) -> Solver.freshVar sp
  -- trace ("vps: " ++ (unpack . toStrict $ pShow vps)) $ pure ()
  Ctx.push
  Ctx.define (snd (value n)) (Scheme vps v)
  ps' <- forM (zip ps vps) $ \ ~(p, pv) -> genPatternConstraints p (TVar pv) False
  -- trace ("ps: " ++ (unpack . toStrict $ pShow ps')) $ pure ()
  e'@(Typed _ te) <- genExprConstraints e
  -- trace ("e: " ++ (unpack . toStrict $ pShow e')) $ pure ()
  b'@(Typed _ tb) <- genExprConstraints b
  -- trace ("b: " ++ (unpack . toStrict $ pShow b')) $ pure ()
  let ty = foldr (\pv t -> TArrow (TVar pv) t) te vps
  Solver.pushConstraint $ Eq v ty
  -- trace ("ty: " ++ (unpack . toStrict $ pShow ty)) $ pure ()
  Ctx.pop
  pure $ Typed (Spanned (EFn n ps' e' b') s) tb
genExprConstraints (Spanned (N.EIf c t e) s) = do
  c'@(Typed _ tc) <- genExprConstraints c
  t'@(Typed _ tt) <- genExprConstraints t
  e'@(Typed _ te) <- genExprConstraints e
  Solver.pushConstraint $ Eq tc TBool
  Solver.pushConstraint $ Eq tt te
  pure $ Typed (Spanned (EIf c' t' e') s) tt
genExprConstraints (Spanned (N.EMatch e cs) s) = do
  e'@(Typed _ te) <- genExprConstraints e
  v <- TVar <$> Solver.freshVar s
  cs' <- forM cs $ \(p, b) -> do
    Ctx.push
    p' <- genPatternConstraints p te False
    b'@(Typed _ tb') <- genExprConstraints b
    Solver.pushConstraint $ Eq v tb'
    Ctx.pop
    pure (p', b')
  pure $ Typed (Spanned (EMatch e' cs') s) v
genExprConstraints (Spanned (N.EList es) s) = do
  v <- TVar <$> Solver.freshVar s
  es' <- forM es $ \e -> genExprConstraints e
  forM_ es' $ \ ~(Typed _ t) -> Solver.pushConstraint $ Eq v t
  pure $ Typed (Spanned (EList es') s) (TList v)
genExprConstraints (Spanned (N.EArray es) s) = do
  v <- TVar <$> Solver.freshVar s
  es' <- forM es $ \e -> genExprConstraints e
  forM_ es' $ \ ~(Typed _ t) -> Solver.pushConstraint $ Eq v t
  pure $ Typed (Spanned (EArray es') s) (TArray v)
-- genExprConstraints (Spanned (N.ETuple es) s) = do
--   es' <- forM es $ \e -> genExprConstraints e
--   pure $ Typed (Spanned (ETuple es') s) (TTuple $ fmap (Ty.typeOf . value) es')
genExprConstraints e = todo

genPatternConstraints :: Spanned N.Pattern -> Ty -> Bool -> InferState (Typed Pattern)
genPatternConstraints (Spanned N.PWildcard s) _ _ = pure $ Typed (Spanned PWildcard s) TUnit
genPatternConstraints (Spanned (N.PLit (N.LInt i)) s) _ _ = pure $ Typed (Spanned (PLit (LInt i)) s) TInt
genPatternConstraints (Spanned (N.PLit (N.LBool b)) s) _ _ = pure $ Typed (Spanned (PLit (LBool b)) s) TBool
genPatternConstraints (Spanned (N.PLit (N.LString t)) s) _ _ = pure $ Typed (Spanned (PLit (LString t)) s) TString
genPatternConstraints (Spanned (N.PVar n) s) ty False = do
  Ctx.define (snd (value n)) (Scheme [] ty)
  pure $ Typed (Spanned (PVar n) s) ty
genPatternConstraints (Spanned (N.PVar n@(Spanned (_, i) _)) s) ty True = do
  scm <- generalize ty
  Ctx.define i scm
  pure $ Typed (Spanned (PVar n) s) ty
genPatternConstraints (Spanned (N.PPair p1 p2) s) ty gen = do
  v <- TVar <$> Solver.freshVar s
  p1'@(Typed _ t1) <- genPatternConstraints p1 v gen
  p2'@(Typed _ t2) <- genPatternConstraints p2 (TList v) gen
  Solver.pushConstraint $ Eq (TList t1) t2
  pure $ Typed (Spanned (PPair p1' p2') s) (TList v)
genPatternConstraints (Spanned (N.PList ps) s) ty gen = do
  v <- TVar <$> Solver.freshVar s
  ps' <- forM ps $ \p -> do
    genPatternConstraints p v gen
  pure $ Typed (Spanned (PList ps') s) (TList v)
genPatternConstraints (Spanned N.PUnit s) _ _ = pure $ Typed (Spanned PUnit s) TUnit

solveConstraints :: Spanned Program -> InferState (Spanned Program)
solveConstraints p = do
  Solver {constraints = cs, subst = sub} <- get
  go (Constraint.applySubst sub <$> cs) p
  where
    go [] p = pure p
    go (Eq t1 t2 : cs) p = do
      Ty.unify t1 t2
      Solver {subst = sub} <- get
      go (Constraint.applySubst sub <$> cs) (applySubstProgram sub p)

infer :: Spanned N.Program -> InferState (Spanned Program)
infer p = do
  p' <- genConstraints p
  p'' <- solveConstraints p'
  s@Solver {subst = sub, ctx = c, constraints = cs} <- get
  -- trace ("constraints: " ++ (unpack . toStrict $ pShow cs)) $ pure ()
  put s {ctx = Ctx.applySubst sub c, constraints = []}
  pure $ p''

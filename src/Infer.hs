{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Infer where

import Common
import Control.Monad
import Control.Monad.State.Strict
import Control.Placeholder (todo)
import Data.Array
import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Text hiding (concat, unwords, zip)
import Data.Text.Lazy (toStrict)
import Debug.Trace (trace)
import qualified NIR as N
import TIR
import Ty
import Prelude hiding (lookup)

data InferError = UnificationError Ty Ty | Occurs Ty Ty deriving (Show, Eq)

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

data Constraint = Eq Ty Ty deriving (Show, Eq)

instance Pretty Constraint where
  pretty (Eq t1 t2) = pretty t1 <> " = " <> pretty t2

applySubstConstraint :: Subst -> Constraint -> Constraint
applySubstConstraint s (Eq t1 t2) = Eq (applySubstTy s t1) (applySubstTy s t2)

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
      pushError $ UnificationError t1 t2

bind :: Spanned TyVar -> Ty -> InferState ()
bind v t
  | t == TVar v = pure ()
  | Set.member v (freeVarsTy t) = do
      pushError $ Occurs (TVar v) t
  | otherwise = do
      s <- get
      let su = subst s
      -- traceM ("bind " ++ (unpack $ pretty v) ++ " to " ++ (unpack $ pretty t) ++ " gives " ++ (show $ Map.insert v t su))
      put s {subst = Map.insert v t su}

newtype Context = Context [Map Unique Scheme] deriving (Show)

freeVarsCtx :: Context -> Set (Spanned TyVar)
freeVarsCtx (Context fs) =
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

inst :: Scheme -> InferState Ty
inst (Scheme vars t) = do
  vars' <- forM vars $ \(Spanned (TyVar _) sp) -> TVar <$> freshVar sp
  let s = Map.fromList $ zip vars vars'
  pure $ applySubstTy s t

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
  pushConstraint $ Eq tp te
  pure $ Typed (Spanned (DDef p' e') s) te
genDeclConstraints (Spanned (N.DFn n ps e) s) = do
  v <- TVar <$> freshVar s
  vps <- forM ps $ \(Spanned _ sp) -> freshVar sp
  define (snd (value n)) (Scheme vps v)
  push
  ps' <- forM (zip ps vps) $ \ ~(p, pv) -> genPatternConstraints p (TVar pv) False
  e'@(Typed _ te) <- genExprConstraints e
  let ty = List.foldr (\pv t -> TArrow (TVar pv) t) te vps
  pushConstraint $ Eq v ty
  pop
  pure $ Typed (Spanned (DFn n ps' e') s) ty
genDeclConstraints (Spanned (N.DFnMatch n t cs) s) = todo
genDeclConstraints _ = todo

genExprConstraints :: Spanned N.Expr -> InferState (Typed Expr)
genExprConstraints (Spanned (N.ELit (N.LInt i)) s) = pure $ Typed (Spanned (ELit (LInt i)) s) TInt
genExprConstraints (Spanned (N.ELit (N.LBool b)) s) = pure $ Typed (Spanned (ELit (LBool b)) s) TBool
genExprConstraints (Spanned (N.ELit (N.LString t)) s) = pure $ Typed (Spanned (ELit (LString t)) s) TString
genExprConstraints (Spanned (N.EVar n) s) = do
  p <- lookup (snd (value n))
  v <- inst p
  pure $ Typed (Spanned (EVar n) s) v
genExprConstraints (Spanned (N.EApp f arg) s) = do
  ~f'@(Typed _ tf) <- genExprConstraints f
  ~arg'@(Typed _ targ) <- genExprConstraints arg
  v <- TVar <$> freshVar s
  let ty = TArrow targ v
  pushConstraint $ Eq tf ty
  -- trace ("genExprConstraints: EApp " ++ (unpack . toStrict $ pShow t1) ++ "\n" ++ (unpack . toStrict $ pShow (TArrow t2 v))) $
  --   pure ()
  pushConstraint $ Eq tf ty
  pure $ Typed (Spanned (EApp f' arg') s) v
genExprConstraints (Spanned (N.ELam p@(Spanned _ sp) e) s) = do
  push
  v <- TVar <$> freshVar sp
  p' <- genPatternConstraints p v False
  e'@(Typed _ te) <- genExprConstraints e
  pop
  pure $ Typed (Spanned (ELam p' e') s) (TArrow v te)
genExprConstraints (Spanned (N.ELet p e1 e2) s) = do
  e1'@(Typed _ t1) <- genExprConstraints e1
  push
  p'@(Typed _ tp) <- genPatternConstraints p t1 True
  e2'@(Typed _ t2) <- genExprConstraints e2
  pushConstraint $ Eq tp t1
  pop
  pure $ Typed (Spanned (ELet p' e1' e2') s) t2
genExprConstraints (Spanned (N.EFn n ps e b) s) = do
  -- trace "genExprConstraints: EFn" $ pure ()
  v <- TVar <$> freshVar s
  -- trace ("var: " ++ (unpack . toStrict $ pShow v)) $ pure ()
  vps <- forM ps $ \(Spanned _ sp) -> freshVar sp
  -- trace ("vps: " ++ (unpack . toStrict $ pShow vps)) $ pure ()
  push
  define (snd (value n)) (Scheme vps v)
  ps' <- forM (zip ps vps) $ \ ~(p, pv) -> genPatternConstraints p (TVar pv) False
  -- trace ("ps: " ++ (unpack . toStrict $ pShow ps')) $ pure ()
  e'@(Typed _ te) <- genExprConstraints e
  -- trace ("e: " ++ (unpack . toStrict $ pShow e')) $ pure ()
  b'@(Typed _ tb) <- genExprConstraints b
  -- trace ("b: " ++ (unpack . toStrict $ pShow b')) $ pure ()
  let ty = List.foldr (\pv t -> TArrow (TVar pv) t) te vps
  pushConstraint $ Eq v ty
  -- trace ("ty: " ++ (unpack . toStrict $ pShow ty)) $ pure ()
  pop
  pure $ Typed (Spanned (EFn n ps' e' b') s) tb
genExprConstraints (Spanned (N.EIf c t e) s) = do
  c'@(Typed _ tc) <- genExprConstraints c
  t'@(Typed _ tt) <- genExprConstraints t
  e'@(Typed _ te) <- genExprConstraints e
  pushConstraint $ Eq tc TBool
  pushConstraint $ Eq tt te
  pure $ Typed (Spanned (EIf c' t' e') s) tt
genExprConstraints (Spanned (N.EMatch e cs) s) = do
  e'@(Typed _ te) <- genExprConstraints e
  v <- TVar <$> freshVar s
  cs' <- forM cs $ \(p, b) -> do
    push
    p' <- genPatternConstraints p te False
    b'@(Typed _ tb') <- genExprConstraints b
    pushConstraint $ Eq v tb'
    pop
    pure (p', b')
  pure $ Typed (Spanned (EMatch e' cs') s) v
genExprConstraints (Spanned (N.EList es) s) = do
  v <- TVar <$> freshVar s
  es' <- forM es $ \e -> genExprConstraints e
  forM_ es' $ \ ~(Typed _ t) -> pushConstraint $ Eq v t
  pure $ Typed (Spanned (EList es') s) (TList v)
genExprConstraints (Spanned (N.EArray es) s) = do
  v <- TVar <$> freshVar s
  es' <- forM es $ \e -> genExprConstraints e
  forM_ es' $ \ ~(Typed _ t) -> pushConstraint $ Eq v t
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
  define (snd (value n)) (Scheme [] ty)
  pure $ Typed (Spanned (PVar n) s) ty
genPatternConstraints (Spanned (N.PVar n@(Spanned (_, i) _)) s) ty True = do
  scm <- generalize ty
  define i scm
  pure $ Typed (Spanned (PVar n) s) ty
genPatternConstraints (Spanned (N.PPair p1 p2) s) ty gen = do
  v <- TVar <$> freshVar s
  p1'@(Typed _ t1) <- genPatternConstraints p1 v gen
  p2'@(Typed _ t2) <- genPatternConstraints p2 (TList v) gen
  pushConstraint $ Eq (TList t1) t2
  pure $ Typed (Spanned (PPair p1' p2') s) (TList v)
genPatternConstraints (Spanned (N.PList ps) s) ty gen = do
  v <- TVar <$> freshVar s
  ps' <- forM ps $ \p -> do
    genPatternConstraints p v gen
  pure $ Typed (Spanned (PList ps') s) (TList v)
genPatternConstraints (Spanned N.PUnit s) _ _ = pure $ Typed (Spanned PUnit s) TUnit

solveConstraints :: Spanned Program -> InferState (Spanned Program)
solveConstraints p = do
  Solver {constraints = cs, subst = sub} <- get
  go (applySubstConstraint sub <$> cs) p
  where
    go [] p = pure p
    go (Eq t1 t2 : cs) p = do
      unify t1 t2
      Solver {subst = sub} <- get
      go (applySubstConstraint sub <$> cs) (applySubstProgram sub p)

infer :: Spanned N.Program -> InferState (Spanned Program)
infer p = do
  p' <- genConstraints p
  p'' <- solveConstraints p'
  s@Solver {subst = sub, ctx = c, constraints = cs} <- get
  -- trace ("constraints: " ++ (unpack . toStrict $ pShow cs)) $ pure ()
  put s {ctx = applySubstCtx sub c, constraints = []}
  pure $ p''

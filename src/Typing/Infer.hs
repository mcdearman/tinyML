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

generalize :: Ty -> InferState Scheme
generalize t = do
  Solver {ctx = c} <- get
  pure $ Scheme (Set.toList (Ty.freeVars t `Set.difference` Ctx.freeVars c)) t

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

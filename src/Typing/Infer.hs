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
import Text.Pretty.Simple
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
genConstraints (Spanned (N.PRepl (Left d)) s) = do
  d' <- genDeclConstraints d
  pure $ Spanned (PRepl (Left d')) s
genConstraints (Spanned (N.PRepl (Right e)) s) = do
  d' <- genExprConstraints e
  pure $ Spanned (PRepl (Right d')) s
genConstraints (Spanned (N.PFile n m) s) = todo

genModuleConstraints :: Spanned N.Module -> InferState ()
genModuleConstraints m = todo

genDeclConstraints :: Spanned N.Decl -> InferState (Typed Decl)
genDeclConstraints (Spanned (N.DDef p e) s) = do
  -- v <- TVar <$> Solver.freshVar s
  e'@(Typed _ te) <- genExprConstraints e
  p'@(Typed _ tp) <- genPatternConstraints p te True
  Solver.pushConstraint $ Eq tp te
  -- Solver.pushConstraint $ Eq tp v
  pure $ Typed (Spanned (DDef p' e') s) te
genDeclConstraints (Spanned (N.DFn n ps e) s) = do
  v <- TVar <$> Solver.freshVar s
  vps <- forM ps $ \(Spanned _ sp) -> Solver.freshVar sp
  ps' <- forM ps $ \p -> genPatternConstraints p v True
  Ctx.define (snd (value n)) (Scheme vps v)
  e'@(Typed _ te) <- genExprConstraints e
  Solver.pushConstraint $ Eq v (TArrow te v)
  pure $ Typed (Spanned (DFn n ps' e') s) v
genDeclConstraints _ = todo

genExprConstraints :: Spanned N.Expr -> InferState (Typed Expr)
genExprConstraints (Spanned (N.ELit (N.LInt i)) s) = pure $ Typed (Spanned (ELit (LInt i)) s) TInt
genExprConstraints (Spanned (N.ELit (N.LBool b)) s) = pure $ Typed (Spanned (ELit (LBool b)) s) TBool
genExprConstraints (Spanned (N.ELit (N.LString t)) s) = pure $ Typed (Spanned (ELit (LString t)) s) TString
genExprConstraints (Spanned (N.EVar n) s) = do
  p <- Ctx.lookup (snd (value n))
  v <- Scheme.inst p
  pure $ Typed (Spanned (EVar n) s) v
genExprConstraints (Spanned (N.EApp e1 e2) s) = do
  e1'@(Typed _ t1) <- genExprConstraints e1
  e2'@(Typed _ t2) <- genExprConstraints e2
  v <- TVar <$> Solver.freshVar s
  -- trace ("genExprConstraints: EApp " ++ (unpack . toStrict $ pShow t1) ++ "\n" ++ (unpack . toStrict $ pShow (TArrow t2 v))) $
  --   pure ()
  Solver.pushConstraint $ Eq t1 (TArrow t2 v)
  pure $ Typed (Spanned (EApp e1' e2') s) v
genExprConstraints (Spanned (N.ELam p@(Spanned _ sp) e) s) = do
  Ctx.push
  v <- TVar <$> Solver.freshVar sp
  p' <- genPatternConstraints p v False
  e'@(Typed _ te) <- genExprConstraints e
  Ctx.pop
  pure $ Typed (Spanned (ELam p' e') s) (TArrow v te)
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
  p1'@(Typed _ t1) <- genPatternConstraints p1 ty gen
  p2'@(Typed _ t2) <- genPatternConstraints p2 ty gen
  Solver.pushConstraint $ Eq (TList t1) t2
  pure $ Typed (Spanned (PPair p1' p2') s) t2
genPatternConstraints (Spanned (N.PList ps) s) ty gen = do
  ps' <- forM ps $ \p -> genPatternConstraints p ty gen
  pure $ Typed (Spanned (PList ps') s) ty
genPatternConstraints (Spanned N.PUnit s) _ _ = pure $ Typed (Spanned PUnit s) TUnit

solveConstraints :: InferState ()
solveConstraints = do
  Solver {constraints = cs} <- get
  forM_ cs $ \case
    Eq t1 t2 -> Ty.unify t1 t2

infer :: Spanned N.Program -> InferState (Spanned Program)
infer p = do
  p' <- genConstraints p
  solveConstraints
  s@Solver {subst = sub, ctx = c} <- get
  put s {ctx = Ctx.applySubst sub c}
  pure $ applySubstProgram sub p'

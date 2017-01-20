module Rock.Syntax
  ( Name(..)
  , Term(..)

  , prettyName
  , prettyTerm

  , substitute

  , alphaRename
  , alphaEquivalent

  , betaReduce
  , betaReduce'
  , evaluate
  , betaEquivalent
  ) where

import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Supply.Class (fresh, class MonadSupply, sneak)
import Control.Plus (empty)
import Data.Map (Map)
import Data.Map as Map
import Rock.Prelude

--------------------------------------------------------------------------------

data Name
  = SourceName String
  | UniqueName Int

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

data Term
  = Var Name
  | App Term Term
  | Abs Name Term Term
  | Pii Name Term Term
  | Let Name Term Term
  | Typ

  -- ignore unless you're @wires
  | Lservice
  | Service

derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term

--------------------------------------------------------------------------------

prettyName :: Name -> String
prettyName (SourceName x) = x
prettyName (UniqueName i) = "$" <> show i

prettyTerm :: Term -> String
prettyTerm (Var x) = prettyName x
prettyTerm (App e1 e2) = "(" <> prettyTerm e1 <> ") (" <> prettyTerm e2 <> ")"
prettyTerm (Abs x t e) = "λ(" <> prettyName x <> " : " <> prettyTerm t <> ") -> " <> prettyTerm e
prettyTerm (Pii x t e) = "Π(" <> prettyName x <> " : " <> prettyTerm t <> ") -> " <> prettyTerm e
prettyTerm (Let x e1 e2) = "let " <> prettyName x <> " = " <> prettyTerm e1 <> " in " <> prettyTerm e2
prettyTerm (Typ) = "⋆"
prettyTerm (Lservice) = "Lservice"
prettyTerm (Service) = "(↝)"

--------------------------------------------------------------------------------

substitute :: Name -> Term -> Term -> Term
substitute x t1 t2@(Var t2X)
  | t2X == x  = t1
  | otherwise = t2
substitute x t1 (App t2E1 t2E2) =
  App (substitute x t1 t2E1) (substitute x t1 t2E2)
substitute x t1 (Abs t2X t2T t2E)
  | t2X == x  = Abs t2X (substitute x t1 t2T) t2E
  | otherwise = Abs t2X (substitute x t1 t2T) (substitute x t1 t2E)
substitute x t1 (Pii t2X t2T t2E)
  | t2X == x  = Pii t2X (substitute x t1 t2T) t2E
  | otherwise = Pii t2X (substitute x t1 t2T) (substitute x t1 t2E)
substitute x t1 (Let t2X t2E1 t2E2)
  | t2X == x  = Let t2X (substitute x t1 t2E1) t2E2
  | otherwise = Let t2X (substitute x t1 t2E1) (substitute x t1 t2E2)
substitute _ _  (Typ) = Typ
substitute _ _  (Lservice) = Lservice
substitute _ _  (Service) = Service

--------------------------------------------------------------------------------

alphaRename :: ∀ m. (MonadSupply Name m) => Term -> m Term
alphaRename = go Map.empty
  where
  go :: Map Name Name -> Term -> m Term
  go g (Var x) = pure $ Var (fromMaybe x (Map.lookup x g))
  go g (App e1 e2) = App <$> go g e1 <*> go g e2
  go g (Abs x t e) = do
    x' <- fresh
    Abs x' <$> go g t <*> go (Map.insert x x' g) e
  go g (Pii x t e) = do
    x' <- fresh
    Pii x' <$> go g t <*> go (Map.insert x x' g) e
  go g (Let x e1 e2) = do
    x' <- fresh
    Let x' <$> go g e1 <*> go (Map.insert x x' g) e2
  go _ (Typ) = pure Typ
  go _ (Lservice) = pure Lservice
  go _ (Service) = pure Service

alphaEquivalent :: ∀ m. (MonadSupply Name m) => Term -> Term -> m Boolean
alphaEquivalent t u = (==) <$> sneak (alphaRename t) <*> sneak (alphaRename u)

--------------------------------------------------------------------------------

betaReduce :: ∀ m. (MonadSupply Name m) => Term -> m Term
betaReduce (App (Abs x t e) e2) = alphaRename e  <#> substitute x e2
betaReduce (Let x e1 e2)        = alphaRename e2 <#> substitute x e1
betaReduce e = pure e

betaReduce' :: ∀ m. (MonadSupply Name m) => Int -> Term -> MaybeT m Term
betaReduce' n _ | n <= 0 = empty
betaReduce' n e@(App (Abs _ _ _) _) = betaReduce e >>= betaReduce' (n - 1)
betaReduce' n e@(Let _ _ _)         = betaReduce e >>= betaReduce' (n - 1)
betaReduce' _ e = pure e

evaluate :: ∀ m. (MonadSupply Name m) => Int -> Term -> MaybeT m Term
evaluate n _ | n <= 0 = empty
evaluate _ (Var x) = pure $ Var x
evaluate n (App e1 e2) = do
  e1' <- evaluate (n - 1) e1
  e2' <- evaluate (n - 1) e2
  betaReduce' n (App e1' e2')
evaluate n (Abs x t e) =
  Abs x <$> evaluate (n - 1) t <*> evaluate (n - 1) e
evaluate n (Pii x t e) =
  Pii x <$> evaluate (n - 1) t <*> evaluate (n - 1) e
evaluate n (Let x e1 e2) =
  (Let x <$> evaluate (n - 1) e1 <*> evaluate (n - 1) e2)
  >>= betaReduce' n
evaluate _ (Typ) = pure Typ
evaluate _ (Lservice) = pure Lservice
evaluate _ (Service) = pure Service

betaEquivalent :: ∀ m. (MonadSupply Name m) => Int -> Term -> Term -> MaybeT m Boolean
betaEquivalent n t u = do
  t' <- evaluate n t
  u' <- evaluate n u
  alphaEquivalent t' u'

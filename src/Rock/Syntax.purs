module Rock.Syntax
  ( Name(..)
  , Term(..)
  , Literal(..)

  , prettyName
  , prettyTerm
  , prettyLiteral

  , substitute
  , substituteAll

  , alphaRename
  , alphaEquivalent

  , betaReduce
  , betaReduce'
  , evaluate
  , evaluate'
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
  | IntrinsicName String

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

data Term
  = Var Name
  | App Term Term
  | Abs Name Term Term
  | Pii Name Term Term
  | Let Name Term Term
  | Lit Literal
  | Typ

derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term

data Literal
  = Bool Boolean
  | Int Int
  | String String

derive instance eqLiteral :: Eq Literal
derive instance ordLiteral :: Ord Literal

--------------------------------------------------------------------------------

prettyName :: Name -> String
prettyName (SourceName x) = x
prettyName (UniqueName i) = "$" <> show i
prettyName (IntrinsicName x) = "#" <> x

prettyTerm :: Term -> String
prettyTerm = go
  where
  go (Var x) = prettyName x
  go (App e1 e2) = parens 2 e1 <> " " <> parens 1 e2
  go (Abs x t e) = "λ(" <> prettyName x <> " : " <> parens 3 t <> ") -> " <> parens 3 e
  go (Pii x t e) = "Π(" <> prettyName x <> " : " <> parens 3 t <> ") -> " <> parens 3 e
  go (Let x e1 e2) = "let " <> prettyName x <> " = " <> parens 3 e1 <> " in " <> parens 3 e2
  go (Lit l) = prettyLiteral l
  go (Typ) = "⋆"

  parens l t
    | level t > l = "(" <> go t <> ")"
    | otherwise   = go t

  level (Var _) = 1
  level (App _ _) = 2
  level (Abs _ _ _) = 3
  level (Pii _ _ _) = 3
  level (Let _ _ _) = 3
  level (Lit _) = 1
  level (Typ) = 1

prettyLiteral :: Literal -> String
prettyLiteral (Bool b)   = if b then "true" else "false"
prettyLiteral (Int i)    = show i
prettyLiteral (String s) = show s

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
substitute _ _  (Lit l) = Lit l
substitute _ _  (Typ) = Typ

substituteAll :: ∀ f. (Foldable f) => f (Tuple Name Term) -> Term -> Term
substituteAll f t = foldr (uncurry substitute) t f

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
  go _ (Lit l) = pure (Lit l)
  go _ (Typ) = pure Typ

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
evaluate _ (Lit l) = pure (Lit l)
evaluate _ (Typ) = pure Typ

evaluate' :: ∀ m. (MonadSupply Name m) => Int -> Int -> Term -> MaybeT m Term
evaluate' n _ _ | n <= 0 = empty
evaluate' n m t = do
  t' <- evaluate m t
  if t' == t
    then pure t'
    else evaluate' (n - 1) m t'

betaEquivalent :: ∀ m. (MonadSupply Name m) => Int -> Term -> Term -> MaybeT m Boolean
betaEquivalent n t u = do
  t' <- evaluate' n n t
  u' <- evaluate' n n u
  alphaEquivalent t' u'

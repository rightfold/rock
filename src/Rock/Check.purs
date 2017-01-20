module Rock.Check
  ( Error(..)

  , Check(..)
  , runCheck

  , infer
  ) where

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Class as State
import Control.Monad.State.Trans (evalStateT, StateT)
import Control.Monad.Supply.Class (class MonadSupply)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Rock.Prelude
import Rock.Syntax (betaEquivalent, Name(..), substitute, Term(..))

--------------------------------------------------------------------------------

recursionDepth :: Int
recursionDepth = 128

--------------------------------------------------------------------------------

data Error = Error

--------------------------------------------------------------------------------

newtype Check a = Check (StateT Int (ExceptT Error Identity) a)

derive newtype instance functorCheck     :: Functor Check
derive newtype instance applyCheck       :: Apply Check
derive newtype instance applicativeCheck :: Applicative Check
derive newtype instance bindCheck        :: Bind Check
derive newtype instance monadCheck       :: Monad Check
derive newtype instance monadErrorCheck  :: MonadError Error Check
derive newtype instance monadStateCheck  :: MonadState Int Check

instance monadSupplyCheck :: MonadSupply Name Check where
  sneak action = do
    original <- State.get
    result <- action
    State.put original
    pure result
  fresh = do
    State.modify (_ + 1)
    UniqueName <$> State.get

runCheck :: ∀ a. Check a -> Either Error a
runCheck (Check action) = case runExceptT (evalStateT action 0) of Identity r -> r

--------------------------------------------------------------------------------

infer :: Map Name {definition :: Term, type :: Term} -> Term -> Check Term
infer g (Var x) = maybe (throwError Error) (pure <<< _.type) (Map.lookup x g)
infer g (App e1 e2) = do
  e1Type <- infer g e1
  e2Type <- infer g e2
  case e1Type of
    Pii x t e -> do
      infer g t
      runMaybeT (betaEquivalent recursionDepth t e2Type) >>= case _ of
        Just true  -> pure $ substitute x e2 e
        Just false -> throwError Error
        Nothing    -> throwError Error
    _ -> throwError Error
infer g (Abs x t e) = do
  infer g t
  let g' = Map.insert x {definition: Var x, type: t} g
  Pii x t <$> infer g' e
infer g (Pii x t e) = do
  infer g t
  let g' = Map.insert x {definition: Var x, type: t} g
  infer g' e
  pure Typ
infer g (Let x e1 e2) = do
  e1Type <- infer g e1
  let g' = Map.insert x {definition: e1, type: e1Type} g
  infer g' e2
infer _ (Typ) = pure Typ

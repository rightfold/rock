module Rock.Check
  ( Error(..)
  , Check(..)
  , runCheck
  ) where

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Class as State
import Control.Monad.State.Trans (evalStateT, StateT)
import Control.Monad.Supply.Class (class MonadSupply)
import Data.Identity (Identity(..))
import Rock.Prelude
import Rock.Syntax (Name(..))

--------------------------------------------------------------------------------

data Error

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

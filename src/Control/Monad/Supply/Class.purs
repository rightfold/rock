module Control.Monad.Supply.Class
  ( class MonadSupply
  , sneak
  , fresh
  ) where

import Control.Monad.Maybe.Trans (MaybeT(..))
import Rock.Prelude

class (Monad m) <= MonadSupply a m | m -> a where
  sneak :: ∀ b. m b -> m b
  fresh :: m a

instance monadSupplyMaybeT :: (MonadSupply a m) => MonadSupply a (MaybeT m) where
  sneak (MaybeT a) = MaybeT $ sneak a
  fresh = MaybeT (Just <$> fresh)

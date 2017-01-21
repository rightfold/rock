module Data.Number.ReflNaN
  ( ReflNaN(..)
  ) where

import Rock.Prelude

newtype ReflNaN = ReflNaN Number

instance eqReflNaN :: Eq ReflNaN where
  eq (ReflNaN a) (ReflNaN b)
    | isNaN a && isNaN b = true
    | otherwise          = a == b

instance ordReflNaN :: Ord ReflNaN where
  compare (ReflNaN a) (ReflNaN b)
    | isNaN a && isNaN b = EQ
    | otherwise          = a `compare` b

derive newtype instance semiringReflNaN        :: Semiring ReflNaN
derive newtype instance ringReflNaN            :: Ring ReflNaN
derive newtype instance euclideanRingReflNaN   :: EuclideanRing ReflNaN
derive newtype instance commutativeRingReflNaN :: CommutativeRing ReflNaN
derive newtype instance fieldReflNaN           :: Field ReflNaN

derive instance newtypeReflNaN :: Newtype ReflNaN _

isNaN :: Number -> Boolean
isNaN a = a /= a

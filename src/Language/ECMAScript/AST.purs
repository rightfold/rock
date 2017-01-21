module Language.ECMAScript.AST where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Pattern    :: Type
foreign import data Expression :: Type
foreign import data Statement  :: Type
foreign import data Identifier :: Type

foreign import callExpression     :: Expression -> Array Expression -> Expression
foreign import functionExpression :: Array Pattern -> Array Statement -> Expression

foreign import returnStatement :: Expression -> Statement

foreign import identifier :: String -> Identifier

foreign import booleanLiteral :: Boolean -> Expression
foreign import intLiteral     :: Int     -> Expression
foreign import numberLiteral  :: Number  -> Expression
foreign import stringLiteral  :: String  -> Expression
foreign import nullLiteral    :: Expression

identifierPattern :: Identifier -> Pattern
identifierPattern = unsafeCoerce

identifierExpression :: Identifier -> Expression
identifierExpression = unsafeCoerce

foreign import prettyExpression :: Expression -> String

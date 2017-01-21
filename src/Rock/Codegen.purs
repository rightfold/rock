module Rock.Codegen
  ( codegenName
  , codegenTerm
  , codegenLiteral
  ) where

import Language.ECMAScript.AST as E
import Rock.Prelude
import Rock.Syntax (Literal(..), Name(..), Term(..))

--------------------------------------------------------------------------------

codegenName :: Name -> E.Identifier
codegenName (SourceName x) = E.identifier x
codegenName (UniqueName n) = E.identifier ("$" <> show n)
codegenName (IntrinsicName n) = E.identifier ("$$" <> n)

codegenTerm :: Term -> E.Expression
codegenTerm (Var x) = E.identifierExpression (codegenName x)
codegenTerm (App e1 e2) = E.callExpression (codegenTerm e1) [codegenTerm e2]
codegenTerm (Abs x _ e) = E.functionExpression [E.identifierPattern (codegenName x)]
                                               [E.returnStatement (codegenTerm e)]
codegenTerm (Pii _ _ _) = E.nullLiteral
codegenTerm (Let x e1 e2) = E.callExpression f [a]
  where f = E.functionExpression [E.identifierPattern (codegenName x)]
                                 [E.returnStatement (codegenTerm e2)]
        a = codegenTerm e1
codegenTerm (Lit l) = codegenLiteral l
codegenTerm (Typ) = E.nullLiteral

codegenLiteral :: Literal -> E.Expression
codegenLiteral (Bool b)   = E.booleanLiteral b
codegenLiteral (Int i)    = E.intLiteral i
codegenLiteral (Double n) = E.numberLiteral (unwrap n)
codegenLiteral (String s) = E.stringLiteral s

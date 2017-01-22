module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Map as Map
import Language.ECMAScript.AST as E
import Rock.Check (Error(..), infer, runCheck)
import Rock.Codegen (codegenTerm)
import Rock.Prelude
import Rock.Syntax (Literal(..), Name(..), prettyName, prettyTerm, Term(..))

main :: ∀ eff. Eff (console :: CONSOLE | eff) Unit
main = do
  example $ abs "t" typ (abs "x" (var "t") (var "x"))
  example $ app (abs "t" typ (abs "x" (var "t") (var "x"))) (pii "x" typ typ)
  example $ app (app (abs "t" typ (abs "x" (var "t") (var "x"))) (pii "x" typ typ)) (abs "x" typ (var "x"))
  example $ app (app (abs "t" typ (abs "x" (var "t") (var "x"))) (pii "x" typ typ)) (abs "y" typ (var "y"))
  example $ app (app (abs "t" typ (abs "x" (var "t") (var "x"))) (var "bool")) (lit (Bool true))
  example $ le_ "b" (var "bool") (app (abs "x" (var "b") (var "x")) (lit (Bool true)))
  example $ le_ "bool" (var "bool") (app (abs "x" (var "bool") (var "x")) (lit (Bool true)))
  example $ lit (Double (wrap 3.14))
  example $ le_ "void" (app (var "fin") (lit (Int 0))) (app (abs "a" (var "void") (lit (Bool true))) (Fin 0 0))
  example $ le_ "unit" (app (var "fin") (lit (Int 1))) (app (abs "a" (var "unit") (lit (Bool true))) (Fin 1 0))
  example $ le_ "unit" (app (var "fin") (lit (Int 1))) (app (abs "a" (var "unit") (lit (Bool true))) (Fin 1 1))
  where
  example e = do
    log $ prettyTerm e
    case runCheck (infer prelude e) of
      Right t -> do
        log $ "  : " <> prettyTerm t
        log $ E.prettyExpression $ codegenTerm e
      Left (NameError n)         -> log $ "NAME ERROR\n  " <> prettyName n
      Left (MismatchError t1 t2) -> log $ "MISTMATCH ERROR\n  " <> prettyTerm t1 <> "\n  " <> prettyTerm t2
      Left (RecursionError)      -> log $ "RECURSION ERROR"
      Left (PiError t)           -> log $ "PI ERROR\n  " <> prettyTerm t
      Left (FinError b n)        -> log $ "FIN ERROR\n  " <> show b <> " > " <> show n
    log "----------------------------------------"
  prelude =
    Map.empty
    # Map.insert (SourceName "bool")      {definition: Var (IntrinsicName "bool"),   type: typ}
    # Map.insert (SourceName "int")       {definition: Var (IntrinsicName "int"),    type: typ}
    # Map.insert (SourceName "string")    {definition: Var (IntrinsicName "string"), type: typ}
    # Map.insert (SourceName "fin")       {definition: Var (IntrinsicName "fin"),    type: pii "n" (Var (IntrinsicName "int")) typ}
    # Map.insert (IntrinsicName "bool")   {definition: Var (IntrinsicName "bool"),   type: typ}
    # Map.insert (IntrinsicName "int")    {definition: Var (IntrinsicName "int"),    type: typ}
    # Map.insert (IntrinsicName "string") {definition: Var (IntrinsicName "string"), type: typ}
    # Map.insert (IntrinsicName "fin")    {definition: Var (IntrinsicName "fin"),    type: pii "n" (Var (IntrinsicName "int")) typ}
  var x = Var (SourceName x)
  app e1 e2 = App e1 e2
  abs x t e = Abs (SourceName x) t e
  pii x t e = Pii (SourceName x) t e
  le_ x e1 e2 = Let (SourceName x) e1 e2
  fin b n = Fin b n
  lit l = Lit l
  typ = Typ

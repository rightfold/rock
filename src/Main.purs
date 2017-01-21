module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Map as Map
import Rock.Check (infer, runCheck)
import Rock.Prelude
import Rock.Syntax (Literal(..), Name(..), prettyTerm, Term(..))

main :: ∀ eff. Eff (console :: CONSOLE | eff) Unit
main = do
  example $ abs "t" typ (abs "x" (var "t") (var "x"))
  example $ app (abs "t" typ (abs "x" (var "t") (var "x"))) (pii "x" typ typ)
  example $ app (app (abs "t" typ (abs "x" (var "t") (var "x"))) (pii "x" typ typ)) (abs "x" typ (var "x"))
  example $ app (app (abs "t" typ (abs "x" (var "t") (var "x"))) (pii "x" typ typ)) (abs "y" typ (var "y"))
  example $ app (app (abs "t" typ (abs "x" (var "t") (var "x"))) (var "bool")) (lit (Bool true))
  where
  example e = do
    log $ prettyTerm e
    case runCheck (infer prelude e) of
      Right t -> log $ "  : " <> prettyTerm t
      Left  _ -> log "TYPE ERROR"
    log "----------------------------------------"
  prelude =
    Map.empty
    # Map.insert (SourceName "bool")      {definition: Var (IntrinsicName "bool"),   type: Typ}
    # Map.insert (SourceName "int")       {definition: Var (IntrinsicName "int"),    type: Typ}
    # Map.insert (SourceName "string")    {definition: Var (IntrinsicName "string"), type: Typ}
    # Map.insert (IntrinsicName "bool")   {definition: Var (IntrinsicName "bool"),   type: Typ}
    # Map.insert (IntrinsicName "int")    {definition: Var (IntrinsicName "int"),    type: Typ}
    # Map.insert (IntrinsicName "string") {definition: Var (IntrinsicName "string"), type: Typ}
  var x = Var (SourceName x)
  app e1 e2 = App e1 e2
  abs x t e = Abs (SourceName x) t e
  pii x t e = Pii (SourceName x) t e
  lit l = Lit l
  typ = Typ

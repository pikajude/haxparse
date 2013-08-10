module HaxParse.Output (
  outputWith
) where

import           HaxParse.AST
import qualified HaxParse.Output.Plain as Plain
import           HaxParse.Options

outputWith :: OutputType -> Replay -> IO ()
outputWith Plain x = Plain.render x
outputWith JSON _ = error "no JSON renderer yet"

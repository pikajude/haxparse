module HaxParse.Output (
  outputWith
) where

import           HaxParse.AST
import qualified HaxParse.Output.Plain as Plain
import           HaxParse.Options

outputWith :: Opts -> Replay -> IO ()
outputWith opts x = case outputType opts of JSON -> error "no JSON renderer yet"
                                            _ -> Plain.render opts x

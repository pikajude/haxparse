module HaxParse.Output (
  outputWith
) where

import qualified HaxParse.Output.Plain as Plain
import           HaxParse.Options

outputWith Plain x = putStrLn $ Plain.render x
outputWith JSON _ = error "no JSON renderer yet"

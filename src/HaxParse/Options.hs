module HaxParse.Options (
  OutputType(..),
  Opts(..),
  fullOpts
) where

import Data.Char
import Options.Applicative

data OutputType = Plain | JSON deriving Show

data Opts = Opts { outputType :: Maybe OutputType
                 , file :: FilePath
                 } deriving Show

outputTypeReader :: String -> Either ParseError OutputType
outputTypeReader s
    | map toLower s == "json" = Right JSON
    | map toLower s == "plain" = Right Plain
    | otherwise = Left . ErrorMsg $ "unknown output type " ++ s

opts :: Parser Opts
opts = Opts <$> optional (nullOption ( long "output-type"
                                    <> metavar "TYPE"
                                    <> help "Set output type: one of plain [default], json"
                                    <> reader outputTypeReader ))
            <*> argument str (metavar "FILE")

fullOpts :: ParserInfo Opts
fullOpts = info (helper <*> opts)
                (fullDesc <> progDesc "Parse .hbr files.")

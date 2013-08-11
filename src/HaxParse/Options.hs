{-# OPTIONS_GHC -fno-warn-orphans #-}

module HaxParse.Options (
  OutputType(..),
  Opts(..),
  TimeFormat(..),
  fullOpts
) where

import Data.Char
import Data.List.Split
import HaxParse.AST        hiding (Left, Right)
import Options.Applicative

data OutputType = Plain | JSON deriving Show

data TimeFormat = Frame | Second | Both deriving Show

type EventFilter = Action -> Bool

instance Show EventFilter where show _ = "(filter)"

data Opts = Opts { outputType  :: OutputType
                 , showRoom    :: Bool
                 , showDiscs   :: Bool
                 , showPlayers :: Bool
                 , showEvents  :: Bool
                 , timeFormat  :: TimeFormat
                 , file        :: FilePath
                 , eventTypes  :: [EventFilter]
                 } deriving Show

outputTypeReader :: String -> Either ParseError OutputType
outputTypeReader s
    | map toLower s == "json" = Right JSON
    | map toLower s == "plain" = Right Plain
    | otherwise = Left . ErrorMsg $ "unknown output type " ++ s

eventTypeReader :: String -> Either ParseError [EventFilter]
eventTypeReader evs = sequence . map (fmap (. actEvent) . eventTypeReaderSingle . trim) $ splitOn "," evs
    where trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

eventTypeReaderSingle :: String -> Either ParseError (Event -> Bool)
eventTypeReaderSingle s
    | map toLower s `elem` ["new-player", "np"]     = Right isNewPlayer
    | map toLower s `elem` ["departure", "d"]       = Right isDeparture
    | map toLower s `elem` ["change-avatar", "ca"]  = Right isChangeAvatar
    | map toLower s `elem` ["chat", "c"]            = Right isChat
    | map toLower s `elem` ["start-match", "start"] = Right isStartMatch
    | map toLower s `elem` ["stop-match", "stop"]   = Right isStopMatch
    | map toLower s `elem` ["team-change", "tc"]    = Right isTeamChange
    | map toLower s `elem` ["disc-move", "m"]       = Right isDiscMove
    | map toLower s `elem` ["ping-broadcast", "p"]  = Right isPingBroadcast
    | map toLower s `elem` ["time-update", "t"]     = Right isTimeUpdate
    | otherwise = Left . ErrorMsg $ "unknown event type " ++ s

timeFormatReader :: String -> Either ParseError TimeFormat
timeFormatReader x
    | map toLower x == "frame" = Right Frame
    | map toLower x == "second" = Right Second
    | map toLower x == "both" = Right Both
    | otherwise = Left . ErrorMsg $ "unknown time format " ++ x

opts :: Parser Opts
opts = Opts <$> nullOption ( long "output-type"
                          <> short 'o'
                          <> metavar "TYPE"
                          <> help "Set output type: one of plain [default], json"
                          <> reader outputTypeReader
                          <> value Plain )
            <*> switch ( long "show-room"
                      <> short 'r'
                      <> help "Dump room information"
                      <> value False )
            <*> switch ( long "show-discs"
                      <> short 'd'
                      <> help "Dump disc information"
                      <> value False )
            <*> switch ( long "show-players"
                      <> short 'p'
                      <> help "Dump player information"
                      <> value False )
            <*> switch ( long "show-events"
                      <> short 'e'
                      <> help "Dump event information"
                      <> value False )
            <*> nullOption ( long "time-format"
                          <> short 't'
                          <> metavar "FORMAT"
                          <> help "Change timestamp format: frame, second, or both"
                          <> reader timeFormatReader
                          <> value Frame )
            <*> argument str (metavar "FILE")
            <*> nullOption ( long "event-types"
                          <> short 'f'
                          <> metavar "TYPE"
                          <> help "Filter events by type: new-player (np), departure (d), change-avatar (ca), chat (c), start-match (start), stop-match (stop), team-change (tc), disc-move (m), ping-broadcast (p), time-update (t). This option implies --show-events."
                          <> reader eventTypeReader
                          <> value [] )

fullOpts :: ParserInfo Opts
fullOpts = info (helper <*> opts)
                (fullDesc <> progDesc "Parse .hbr files.")

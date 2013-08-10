module HaxParse.AST where

import Data.ByteString.Lazy
import Data.Word

data Event = Event deriving Show

data Side = Red | Blue deriving Show

data Stadium = Classic | Easy | Small | Big
             | Rounded | Hockey | BigHockey | BigEasy
             | BigRounded | Huge | Custom
             deriving (Bounded, Eq, Enum, Ord, Show)

data Color = Color String deriving Show

data Mask = Mask [String] deriving Show

data Disc = Disc { discId       :: Word32
                 , pos          :: (Double, Double)
                 , speed        :: (Double, Double)
                 , radius       :: Double
                 , bCoefficient :: Double
                 , invMass      :: Double
                 , damping      :: Double
                 , color        :: Color
                 , mask         :: Mask
                 , group        :: Mask
                 } deriving (Show)

data Room = Room { roomName     :: ByteString
                 , locked       :: Bool
                 , scoreLimit   :: Word8
                 , timeLimit    :: Word8
                 , rules        :: Word32
                 , kickoffTaken :: Bool
                 , kickoffSide  :: Side
                 , ballCoords   :: (Double, Double)
                 , redScore     :: Word32
                 , blueScore    :: Word32
                 , timer        :: Double
                 , pauseTimer   :: Word8
                 , stadium      :: Stadium
                 } deriving (Show)

data Player = Player { playerId :: Word32
                     , name     :: ByteString
                     , admin :: Bool
                     , team :: Side
                     , number :: Word8
                     , avatar :: ByteString
                     , input :: Word32
                     , autoKick :: Bool
                     , desync :: Bool
                     , country :: ByteString
                     , handicap :: Word16
                     , pDiscId :: Word32
                     } deriving (Show)

data Replay = Replay { version    :: Word32
                     , frameCount :: Word32
                     , firstFrame :: Word32
                     , room       :: Room
                     , inProgress :: Bool
                     , discs      :: [Disc]
                     , players    :: [Player]
                     , events     :: [Event]
                     } deriving Show

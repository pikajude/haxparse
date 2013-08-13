module HaxParse.AST where

import Data.ByteString.Lazy
import Data.IntMap
import Data.Word
import HaxParse.AST.TH

data Move = Nop | Move [Direction] | Kick | MoveKick [Direction] deriving (Eq, Show)

data Direction = Up | Left | Down | Right deriving (Eq, Show)

data Action = Action { actPlayerId   :: Key
                     , actFrameCount :: Word32
                     , actEvent      :: Event
                     } deriving (Eq, Show)

data Side = Red | Blue | Spec deriving (Eq, Show)

data Event = NewPlayer { npId      :: Key
                       , npName    :: ByteString
                       , npAdmin   :: Bool
                       , npCountry :: ByteString
                       }
           | Departure { dId    :: Word32
                       , kicked :: Bool
                       , banned :: Bool
                       , reason :: Maybe ByteString
                       }
           | ChangeAvatar ByteString
           | Chat ByteString
           | StartMatch
           | StopMatch
           | TeamChange Word32 Side
           | DiscMove Move
           | PingBroadcast [(Word32, Word8)]
           | TimeUpdate deriving (Eq, Show)

makeIsFns ''Event

data Stadium = Classic | Easy | Small | Big
             | Rounded | Hockey | BigHockey | BigEasy
             | BigRounded | Huge | Custom
             deriving (Bounded, Eq, Enum, Ord, Show)

data Color = Color String deriving (Eq, Show)

data Mask = Mask [String] deriving (Eq, Show)

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
                 } deriving (Eq, Show)

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
                 } deriving (Eq, Show)

data Player = Player { name     :: ByteString
                     , initial  :: Bool
                     , admin    :: Bool
                     , team     :: Side
                     , number   :: Word8
                     , avatar   :: ByteString
                     , input    :: Word32
                     , autoKick :: Bool
                     , desync   :: Bool
                     , country  :: ByteString
                     , handicap :: Word16
                     , pDiscId  :: Word32
                     } deriving (Eq, Show)

data Replay = Replay { version    :: Word32
                     , frameCount :: Word32
                     , firstFrame :: Word32
                     , room       :: Room
                     , inProgress :: Bool
                     , discs      :: [Disc]
                     , players    :: IntMap Player
                     , events     :: [Action]
                     } deriving (Eq, Show)

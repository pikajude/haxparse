{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module HaxParse.Parser where

import           Codec.Compression.Zlib
import           Control.Applicative        ((<$>), (<*>), pure)
import           Control.Lens               hiding (Action)
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char
import           Data.Default
import qualified Data.IntMap                as I
import           Data.Monoid
import           Data.Word
import           HaxParse.AST
import           Numeric
import           Prelude                    hiding (Left, Right)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Unsafe.Coerce

data ParserState = ParserState { _frame      :: Word32
                               , _curDiscId  :: Word32
                               , _playerList :: I.IntMap Player
                               }

instance Default ParserState where def = ParserState 0 0 mempty

makeLenses ''ParserState

type Parser = Parsec C.ByteString ParserState

instance (Monad m) => Stream C.ByteString m Char where
    uncons = return . C.uncons

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname = do inp <- C.readFile fname
                           return (runP p def fname inp)

parseFile :: FilePath -> IO (Either ParseError Replay)
parseFile m = parseFromFile haxParser m

haxParser :: Parser Replay
haxParser = do vers <- int32
               header <- count 4 anyChar
               when (header /= "HBRP") . fail $ "Did not find correct HBR header. Expected HBRP, found " ++ show header
               framecount <- int32
               setInput . decompress =<< getInput
               firstframe <- int32
               r <- room_
               prog <- bool
               d <- if prog then discs_ else pure []
               players_
               count 14 $ char '\NUL'
               ev <- many event
               plist <- fmap (^. playerList) getState
               return Replay { version      = vers
                             , frameCount   = framecount
                             , firstFrame   = firstframe
                             , room         = r
                             , inProgress   = prog
                             , discs        = d
                             , players      = plist
                             , events       = ev
                             }

room_ :: Parser Room
room_ = do roomname <- str
           l <- bool
           scorelimit <- int8
           timelimit <- int8
           r <- int32
           kotaken <- bool
           koside <- side
           ballx <- double
           bally <- double
           redscore <- int32
           bluescore <- int32
           time <- double
           pausetimer <- int8
           st <- stadium_
           return Room { roomName     = roomname
                       , locked       = l
                       , scoreLimit   = scorelimit
                       , timeLimit    = timelimit
                       , rules        = r
                       , kickoffTaken = kotaken
                       , kickoffSide  = koside
                       , ballCoords   = (ballx, bally)
                       , redScore     = redscore
                       , blueScore    = bluescore
                       , timer        = time
                       , pauseTimer   = pausetimer
                       , stadium      = st
                       }


int64 :: Parser Word64
int64 = do s <- count 8 anyChar
           return . sum $ zipWith shiftL (fromIntegral . ord <$> s) [56,48..]

double :: Parser Double
double = unsafeCoerce <$> int64

int32 :: Parser Word32
int32 = do s <- count 4 anyChar
           return . sum $ zipWith shiftL (fromIntegral . ord <$> s) [24,16..]

int16 :: Parser Word16
int16 = do s <- count 2 anyChar
           return . sum $ zipWith shiftL (fromIntegral . ord <$> s) [8,0]

int8 :: Parser Word8
int8 = fromIntegral . ord <$> anyChar

str :: Parser C.ByteString
str = do len <- int16
         C.pack <$> count (fromIntegral len) anyChar

bool :: Parser Bool
bool = do m <- ord <$> anyChar
          case m of 0 -> return False
                    1 -> return True
                    x -> fail $ "Unexpected value for boolean: " ++ show x

side :: Parser Side
side = do s <- int8
          case s of 1 -> return Red
                    2 -> return Blue
                    _ -> return Spec

stadium_ :: Parser Stadium
stadium_ = do st <- int8
              if st < 255 then return $ [minBound..maxBound] !! fromIntegral st
                          else fail $ "Custom stadiums not handled yet!"

discs_ :: Parser [Disc]
discs_ = do s <- int32
            count (fromIntegral s) disc

disc :: Parser Disc
disc = do discid <- view curDiscId <$> getState
          modifyState (curDiscId +~ 1)
          posx <- double
          posy <- double
          speedx <- double
          speedy <- double
          r <- double
          bco <- double
          im <- double
          damp <- double
          col <- Color . flip showHex "" <$> int32
          m <- mask_
          g <- mask_
          return Disc { discId       = discid
                      , pos          = (posx, posy)
                      , speed        = (speedx, speedy)
                      , radius       = r
                      , bCoefficient = bco
                      , invMass      = im
                      , damping      = damp
                      , color        = col
                      , mask         = m
                      , group        = g
                      }

mask_ :: Parser Mask
mask_ = do start <- int32
           if start + 1 == 0 then pure $ Mask ["all"]
                             else return . Mask $ go start []
    where go 0 ms = ms
          go m xs | m .&. 32 /= 0 = go (m .&. (complement 32)) ("wall":xs)
                  | m .&. 16 /= 0 = go (m .&. (complement 16)) ("blueKO":xs)
                  | m .&. 8 /= 0  = go (m .&. (complement 8))  ("redKO":xs)
                  | m .&. 4 /= 0  = go (m .&. (complement 4))  ("blue":xs)
                  | m .&. 2 /= 0  = go (m .&. (complement 2))  ("red":xs)
                  | m .&. 1 /= 0  = go (m .&. (complement 1))  ("ball":xs)
                  | otherwise     = fail $ "Heh??? " ++ show m

players_ :: Parser ()
players_ = do p <- int32
              void $ count (fromIntegral p) player

player :: Parser ()
player = do pid <- fromIntegral <$> int32
            uname <- str
            a <- bool
            t <- side
            n <- int8
            av <- str
            inp <- int32
            ak <- bool
            d <- bool
            ct <- str
            h <- int16
            did <- int32
            let p = Player { name     = uname
                           , initial  = True
                           , admin    = a
                           , team     = t
                           , number   = n
                           , avatar   = av
                           , input    = inp
                           , autoKick = ak
                           , desync   = d
                           , country  = ct
                           , handicap = h
                           , pDiscId  = did
                           }
            modifyState (playerList %~ I.insert pid p)

event :: Parser Action
event = do timeUpdate <- bool <?> "is it a time update?"
           when timeUpdate $ do frames <- int32
                                modifyState (frame +~ frames)
           fc <- view frame <$> getState
           pid <- fromIntegral <$> int32
           evty <- int8
           result <- case evty of 0 -> newPlayer
                                  1 -> departure
                                  2 -> Chat <$> str
                                  4 -> pure StartMatch
                                  5 -> pure StopMatch
                                  6 -> discMove
                                  7 -> TeamChange <$> int32 <*> side
                                  10 -> ChangeAvatar <$> str
                                  15 -> pingBroadcast
                                  x -> fail $ "heh non-exhaustive match for " ++ show x
           return $ Action pid fc result

departure :: Parser Event
departure = do p <- int16
               k <- bool
               r <- if k then Just <$> str else pure Nothing
               b <- bool
               return Departure { dId = fromIntegral p
                                , kicked = k
                                , banned = b
                                , reason = r }

newPlayer :: Parser Event
newPlayer = do i <- fromIntegral <$> int32
               n <- str
               ai <- bool
               ct <- str
               let p = Player { name     = n
                              , initial  = False
                              , admin    = ai
                              , team     = Spec
                              , number   = 0
                              , avatar   = ""
                              , input    = 0
                              , autoKick = False
                              , desync   = False
                              , country  = ct
                              , handicap = 0
                              , pDiscId  = -1
                              }
               modifyState (playerList %~ I.insert i p)
               return $ NewPlayer { npId      = i
                                  , npName    = n
                                  , npAdmin   = ai
                                  , npCountry = ct
                                  }

pingBroadcast :: Parser Event
pingBroadcast = do p <- int8
                   PingBroadcast . zip [0..] . map (*4) <$> count (fromIntegral p) int8

discMove :: Parser Event
discMove = do m <- int8
              return . DiscMove $ [ Nop
                                  , Move [Up]
                                  , Move [Down]
                                  , Move [Up, Down]
                                  , Move [Left]
                                  , Move [Up, Left]
                                  , Move [Down, Left]
                                  , Move [Up, Down, Left]
                                  , Move [Right]
                                  , Move [Up, Right]
                                  , Move [Down, Right]
                                  , Move [Up, Down, Right]
                                  , Move [Left, Right]
                                  , Move [Up, Left, Right]
                                  , Move [Down, Left, Right]
                                  , Move [Up, Down, Left, Right]
                                  , Kick
                                  , MoveKick [Up]
                                  , MoveKick [Down]
                                  , MoveKick [Up, Down]
                                  , MoveKick [Left]
                                  , MoveKick [Up, Left]
                                  , MoveKick [Down, Left]
                                  , MoveKick [Up, Down, Left]
                                  , MoveKick [Right]
                                  , MoveKick [Up, Right]
                                  , MoveKick [Down, Right]
                                  , MoveKick [Up, Down, Right]
                                  , MoveKick [Left, Right]
                                  , MoveKick [Up, Left, Right]
                                  , MoveKick [Down, Left, Right]
                                  , MoveKick [Up, Down, Left, Right]] !! fromIntegral m

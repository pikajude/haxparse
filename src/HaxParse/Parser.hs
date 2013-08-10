module HaxParse.Parser where

import           Codec.Compression.Zlib
import           Control.Applicative         ((<$>), (<*>), pure)
import           Control.Monad
import           Data.Bits
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy.Char8  as C
import           Data.Char
import           Data.Word
import           Debug.Trace
import           HaxParse.AST
import           Numeric
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Unsafe.Coerce

type Parser = Parsec C.ByteString Word32

instance (Monad m) => Stream C.ByteString m Char where
    uncons = return . C.uncons

parseFromFile p fname = do input <- C.readFile fname
                           return (runP p 0 fname input)

parseFile :: FilePath -> IO String
parseFile m = show <$> parseFromFile haxParser m

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
               p <- players_
               getInput >>= flip traceShow (return ())
               return Replay { version      = vers
                             , frameCount   = framecount
                             , firstFrame   = firstframe
                             , room         = r
                             , inProgress   = prog
                             , discs        = d
                             , players      = p
                             , events       = []
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
                    x -> fail $ "Unknown side took kickoff: " ++ show x

stadium_ :: Parser Stadium
stadium_ = do st <- int8
              if st < 255 then return $ [minBound..maxBound] !! fromIntegral st
                          else fail $ "Custom stadiums not handled yet!"

discs_ :: Parser [Disc]
discs_ = do s <- int32
            count (fromIntegral s) disc

disc :: Parser Disc
disc = do discid <- getState
          modifyState succ
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

players_ :: Parser [Player]
players_ = do p <- int32
              count (fromIntegral p) player

player :: Parser Player
player = do pid <- int32
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
            return Player { playerId = pid
                          , name = uname
                          , admin = a
                          , team = t
                          , number = n
                          , avatar = av
                          , input = inp
                          , autoKick = ak
                          , desync = d
                          , country = ct
                          , handicap = h
                          , pDiscId = did
                          }

{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module HaxParse.Output.Plain (
  render
) where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.IntMap               as I
import           HaxParse.AST
import           HaxParse.Options          hiding (showRoom)
import qualified HaxParse.Options          as O
import           Prelude                   hiding (Left, Right)
import           System.IO
import           Text.Printf

type RenderState = ReaderT (Opts, I.IntMap Player) IO

render :: Opts -> Replay -> IO ()
render o re = do
        hSetEncoding stdout utf8
        printf "HBRP version %d.\n" (version re)
        let fullSec = fromIntegral (frameCount re) / 60 :: Double
            minutes = floor (fullSec / 60) :: Integer
            seconds = fullSec - fromIntegral (minutes * 60)
        printf "Length: %d frames (%d:%.3f).\n" (frameCount re) minutes seconds
        printf "First frame: %d.\n" (firstFrame re)
        when (O.showRoom o) $ showRoom $ room re
        printf "The game is %sin progress.\n" $ if inProgress re then "" :: String
                                                                 else "not "
        when (showDiscs o) $ if null $ discs re
                                 then putStrLn "Discs: none."
                                 else putStrLn "Discs:" >> mapM_ showDisc (discs re)
        when (showPlayers o) $ do putStrLn "Players:"
                                  mapM_ showPlayer . I.toList $ players re
        when (showEvents o) $ do putStrLn "Events:"
                                 let filteredEvents = if null (eventTypes o)
                                                          then events re
                                                          else filter (\ev -> any ($ ev) (eventTypes o)) (events re)
                                 forM_ filteredEvents $ \f -> runReaderT (showAction f) (o, players re)

showRoom :: Room -> IO ()
showRoom r = do
        printf "Room name: %s.\n" (toString $ roomName r)
        printf "The room is %slocked.\n" $ if locked r then "" :: String else "not "
        printf "Score limit: %d.\nTime limit: %d min.\n" (scoreLimit r) (timeLimit r)
        printf "Rules? %d.\n" (rules r)
        printf "The kickoff has %sbeen taken%s.\n"
            (if kickoffTaken r then "" :: String else "not ")
            (if kickoffTaken r then " by " ++ show (kickoffSide r) else "")
        uncurry (printf "The ball is at (%.3f,%.3f).\n") (ballCoords r)
        printf "Score: Red %d - %d Blue.\n" (redScore r) (blueScore r)
        printf "Current timer: %.3f%s.\n"
            (timer r)
            (if pauseTimer r > 0 then printf " (unpaused in %d sec)" (pauseTimer r)
                                 else "" :: String)
        showStadium $ stadium r

showStadium :: Stadium -> IO ()
showStadium s = printf "Current stadium: %s.\n" (show s)

showDisc :: Disc -> IO ()
showDisc = undefined

showPlayer :: (I.Key, Player) -> IO ()
showPlayer (k, p) = do
        printf "#%d %s%s\n" k (toString $ name p) (if admin p then " (admin)" :: String else "")
        unless (initial p) $ printf "  This player joined the room during the replay.\n"
        printf "  Team: %s\n" (show $ team p)
        printf "  Number: %d\n" (number p)
        printf "  Avatar: %s\n" (toString $ avatar p)
        printf "  Input: %d\n" (input p)
        printf "  Banned: %s\n" (show $ autoKick p)
        printf "  Desynced: %s\n" (show $ desync p)
        printf "  Country: %s\n" (toString $ country p)
        printf "  Handicap: %d\n" (handicap p)
        printf "  Disc ID: %s\n" (if pDiscId p + 1 == 0 then "(none)" else show (pDiscId p))

showAction :: Action -> RenderState ()
showAction (Action p fc ev) = do
        (o,ps) <- ask
        e <- showEv ev (ps I.! p)
        let prelude = case timeFormat o of
                          Frame -> printf "frame %05d" fc
                          Second -> printf "(%02d:%06.3f)" minutes seconds
                          Both -> printf "frame %05d (%02d:%06.3f)" fc minutes seconds
            sec :: Double
            sec = fromIntegral fc / 60
            minutes :: Integer
            minutes = floor (sec / 60)
            seconds :: Double
            seconds = sec - (fromIntegral minutes * 60)
        liftIO $ printf "%s: %s\n" (prelude :: String) e

showEv :: Event -> Player -> RenderState String
showEv (PingBroadcast ps) _ = return $ printf "ping update: %s" $ show ps
showEv (Chat bs) p = return $ printf "%s: \"%s\"" (toString $ name p) (toString bs)
showEv StartMatch p = return $ printf "%s starts match" (toString $ name p)
showEv (DiscMove ds) p = return $ printf "%s %s" (toString $ name p) (showMoves ds)
showEv (NewPlayer _ n _ c) _ = return $ printf "new player joins: %s (from %s)" (toString n) (toString c)
showEv (ChangeAvatar s) p = return $ printf "%s changes avatar to \"%s\"" (toString $ name p) (toString s)
showEv (Departure d k b r) p = do ps <- asks snd
                                  let n = toString . name $ ps I.! fromIntegral d
                                  if | b -> return $ printf "%s bans %s (%s)" n (toString $ name p) (maybe "no reason" toString r)
                                     | k -> return $ printf "%s kicks %s (%s)" n (toString $ name p) (maybe "no reason" toString r)
                                     | otherwise -> return $ printf "%s leaves" n
showEv StopMatch p = return $ printf "%s stops match" (toString $ name p)
showEv (TeamChange m t) p = do ps <- asks snd
                               let n = toString . name $ ps I.! fromIntegral m
                               return $ printf "%s moves %s to %s" (toString $ name p) n (show t)
showEv x _ = error $ show x

showMoves :: Move -> String
showMoves Kick = "kicks"
showMoves Nop = "stops moving"
showMoves (Move ms) = printf "moves %s" $ join (map showDir ms)
showMoves (MoveKick ms) = printf "moves %s and kicks" $ join (map showDir ms)

showDir :: Direction -> String
showDir Left = "←"
showDir Up = "↑"
showDir Right = "→"
showDir Down = "↓"

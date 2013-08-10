{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module HaxParse.Output.Plain (
  render
) where

import HaxParse.AST
import Text.Printf

render :: Replay -> IO ()
render Replay { version = v
              , frameCount = fc
              , firstFrame = ff } =
    do printf "HBRP version %d.\n" v
       let fullSec = fromIntegral fc / 60 :: Double
           minutes = floor (fullSec / 60) :: Integer
           seconds = fullSec - fromIntegral (minutes * 60)
       printf "Length: %d frames (%d:%.3f).\n" fc minutes seconds
       printf "First frame: %d.\n" ff

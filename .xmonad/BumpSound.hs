#!/usr/bin/env stack
-- stack script --resolver lts-8.12
--   --package turtle
--   --package string-conv

-- Compile like:
-- > stack exec --package turtle --package string-conv -- ghc --make BumpSound.hs

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Control.Foldl as Fold
import Data.String.Conv
import Control.Monad


stepSize = 4
maxSound = 70
minSound = 0


shell' c = shell c empty


parser :: Parser (Text)
parser = argText "direction" "Either 'up' or 'down'."


main :: IO ()
main = void $ do
    cmd <- options "amixer sound bumper" parser
    vol <- currentVolume

    case cmd of
      "down" -> step vol (-stepSize)
      "up"   -> step vol stepSize


step :: Maybe Int -> Int -> IO ()
step Nothing  _  = return ()
step (Just v) s  =  do
    sh $ shell' ("amixer set Master " <> (toS (show nextVol)) <> "%")
    where
        nextVol :: Int
        nextVol = min (max minSound (v+s)) maxSound


currentVolume :: IO (Maybe Int)
currentVolume = do
    v <- flip fold Fold.head $ inshell "amixer get Master | grep '\\[on\\]' | awk '{ print $4 }' | grep -Po '\\d+'" ""
    return $ read . toS . lineToText <$> v

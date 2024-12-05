{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Font
import Linear ()
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  SDL.Font.initialize
  font <- load "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf" 300
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer font
  destroyWindow window

appLoop :: Renderer -> Font -> IO ()
appLoop renderer font = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 0 255
  let rect = Rectangle (P (V2 50 50)) (V2 300 200)
  fillRectF renderer rect
  let fontColor = V4 0 255 0 255
  surface <- solid font fontColor "hello"
  texture <- createTextureFromSurface renderer surface
  let rect = Rectangle (P (V2 50 50)) (V2 300 100)
  copy renderer texture Nothing $ Just rect
  present renderer
  unless qPressed (appLoop renderer font)

{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Font

import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  SDL.Font.initialize
  font <- load "c:\\Windows\\Fonts\\YuGothB.ttc" 300
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
  let rectFill = Rectangle (P (V2 50 50)) (V2 300 200)
  fillRectF renderer rectFill
  let fontColor = V4 0 255 0 255
  surface <- solid font fontColor "hello"
  (V2 intWidth intHeight) <- surfaceDimensions surface
  texture <- createTextureFromSurface renderer surface
  let rectText = Rectangle (P (V2 50 50)) (V2 (div intWidth 8) (div intHeight 8))
  copy renderer texture Nothing $ Just rectText
  present renderer
  unless qPressed (appLoop renderer font)

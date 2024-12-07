{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Font
import Tree (drawTree)

import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  SDL.Font.initialize
  font <- load "c:\\Windows\\Fonts\\YuGothB.ttc" 16
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop True renderer font
  destroyWindow window

appLoop :: Bool -> Renderer -> Font -> IO ()
appLoop firstTime renderer font = do
  events <- pollEvents
  let eventIsQuit event =
        case eventPayload event of
          QuitEvent -> True
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      quitTriggered = any eventIsQuit events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 0 255
  let fontColor = V4 0 255 0 255
  let drawText = solid font fontColor
  drawTree firstTime drawText renderer
  present renderer
  unless quitTriggered (appLoop False renderer font)

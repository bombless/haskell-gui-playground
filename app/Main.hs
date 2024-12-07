{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import SDL
import SDL.Font
import Tree (drawTree)
import Data.Word (Word8)

import Control.Monad (unless)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM)
import Control.Exception (catch)
import Data.Maybe (catMaybes)
import Text.Printf (printf)


-- 函数返回Windows字体目录中的所有字体文件路径
getFontFiles :: IO [FilePath]
getFontFiles = do
    let fontDir = "C:\\Windows\\Fonts"
    allFiles <- listDirectory fontDir
    -- 筛选出字体文件（例如：.ttf, .otf）
    filterM isFontFile (map (fontDir </>) allFiles)

-- 判断一个文件是否是字体文件
isFontFile :: FilePath -> IO Bool
isFontFile path = do
    let ext = takeExtension path
    return (ext == ".ttf" || ext == ".otf")

fontSize :: Int
fontSize = 16

makeFilePair :: FilePath -> IO (Maybe (FilePath, Font))
makeFilePair path = do
  -- 尝试加载字体，如果失败返回 Nothing
  maybePair <- catch (Just <$> load path fontSize) handleError
  case maybePair of
    Nothing -> printf "Loading font failed: %s" path
    _ -> return ()
  return $ fmap (path, ) maybePair

-- 错误处理函数，返回 Nothing
handleError :: SDLException -> IO (Maybe Font)
handleError _ = return Nothing

-- loadFonts，使用 catMaybes 过滤掉 Nothing
loadFonts :: IO [(FilePath, Font)]
loadFonts = do
  files <- getFontFiles
  -- 使用 mapM 和 catMaybes 过滤掉 Nothing
  fonts <- mapM makeFilePair files
  return $ catMaybes fonts

cycleFonts :: IO [(FilePath, Font)]
cycleFonts = cycle <$> loadFonts

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  SDL.Font.initialize
  renderer <- createRenderer window (-1) defaultRenderer
  fonts <- cycleFonts
  appLoop True renderer fonts colors
  destroyWindow window

colors :: [(V4 Word8, V4 Word8, V4 Word8)]
colors = cycle [
  (V4 173 216 230 255, V4 0 0 139 255, V4 255 255 255 255),
  (V4 144 238 144 255, V4 0 128 0 255, V4 240 230 140 255)]

appLoop :: Bool -> Renderer -> [(FilePath, Font)] -> [(V4 Word8, V4 Word8, V4 Word8)] -> IO ()
appLoop _ _ _ [] = undefined
appLoop _ _ [] _ = undefined
appLoop firstTime renderer ((fontPath, font): otherFonts) (colorConfig: otherColors) = do
  events <- pollEvents
  let eventIsQuit event =
        case eventPayload event of
          QuitEvent -> True
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      quitTriggered = any eventIsQuit events
  let eventEnterPressed event =
        case eventPayload event of
          QuitEvent -> True
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeKPEnter
          _ -> False
      enterPressed = any eventEnterPressed events
  let eventTabPressed event =
        case eventPayload event of
          QuitEvent -> True
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeTab
          _ -> False
      tabPressed = any eventTabPressed events
  let (backgroundColor, blockColor, fontColor) = colorConfig
  rendererDrawColor renderer $= backgroundColor
  clear renderer
  rendererDrawColor renderer $= blockColor
  let drawText = blended font fontColor
  drawTree firstTime drawText renderer
  present renderer
  let fontStream = if tabPressed then otherFonts else (fontPath, font): otherFonts
  let colorStream = if enterPressed then otherColors else colorConfig: otherColors
  unless quitTriggered (appLoop False renderer fontStream colorStream)

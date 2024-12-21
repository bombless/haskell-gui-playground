{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Tree (drawTree)
import Data.Word (Word8)

import Control.Monad (unless, filterM, replicateM)
import Control.Exception (catch)
import Data.Maybe (catMaybes)
import Text.Printf (printf)
import Data.Text (pack)

import SDL
import SDL.Font
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.FilePath (takeFileName)
import System.Random


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
  maybeFont <- catch (Just <$> load path fontSize) handleError
  case maybeFont of
    Nothing -> printf "Loading font failed: %s\n" path >> return Nothing
    Just font -> do
      isMono <- isMonospace font
      printsA <- glyphProvided font 'A'
      if isMono && printsA
        then return $ Just (takeFileName path, font)
        else return Nothing

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

randomRInt :: (Int, Int) -> IO Int
randomRInt (minVal, maxVal) = do
  gen <- newStdGen
  return (fst (randomR (minVal, maxVal) gen))

-- 生成一个包含指定数量随机整数的列表
generateRandomList :: Int -> (Int, Int) -> IO [Int]
generateRandomList count range = replicateM count (randomRInt range)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  SDL.Font.initialize
  renderer <- createRenderer window (-1) defaultRenderer
  fonts <- cycleFonts
  numbers <- generateRandomList 10 (0, 999)
  appLoop AppContext { ctxFirstTime = True, ctxWindow = window, ctxRenderer = renderer, ctxNumbers = numbers, ctxFonts = fonts, ctxColors = colors }
  destroyWindow window

colors :: [(V4 Word8, V4 Word8, V4 Word8)]
colors = cycle [
  (V4 173 216 230 255, V4 0 0 139 255, V4 255 255 255 255),
  (V4 144 238 144 255, V4 0 128 0 255, V4 240 230 140 255)]

data AppContext = AppContext {
  ctxFirstTime :: Bool,
  ctxWindow :: Window,
  ctxRenderer :: Renderer,
  ctxNumbers :: [Int],
  ctxFonts :: [(FilePath, Font)],
  ctxColors :: [(V4 Word8, V4 Word8, V4 Word8)]
}

appLoop :: AppContext -> IO ()
appLoop (AppContext {ctxFonts = []}) = undefined
appLoop (AppContext {ctxColors = []}) = undefined
appLoop (ctx@AppContext { ctxRenderer = renderer, ctxFonts = (fontPath, font): otherFonts, ctxColors = colorConfig: otherColors }) = do
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
            elem (keysymKeycode (keyboardEventKeysym keyboardEvent)) [KeycodeKPEnter, KeycodeReturn]
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
  
  case otherFonts of
    ((nextFontPath, nextFont):_) -> do
      let title = printf "current font %s, next font %s" fontPath nextFontPath
      windowTitle (ctxWindow ctx) $= pack title
      do
        surface <- blended font blockColor $ pack $ printf "current: %s" fontPath
        dims <- surfaceDimensions surface
        texture <- createTextureFromSurface renderer surface
        freeSurface surface
        let rectText = Rectangle (P (V2 0 0)) dims
        copy renderer texture Nothing $ Just rectText
        destroyTexture texture

      surface <- blended nextFont blockColor $ pack $ printf "next: %s" nextFontPath
      dims <- surfaceDimensions surface
      texture <- createTextureFromSurface renderer surface
      freeSurface surface
      let rectText = Rectangle (P (V2 0 20)) dims
      copy renderer texture Nothing $ Just rectText
      destroyTexture texture

    _ -> return ()

  drawTree (ctxFirstTime ctx) drawText (ctxNumbers ctx) renderer

  present renderer
  let fontStream = if tabPressed then otherFonts else (fontPath, font): otherFonts
  let colorStream = if enterPressed then otherColors else colorConfig: otherColors
  unless quitTriggered (appLoop ctx { ctxFirstTime = False, ctxFonts = fontStream, ctxColors = colorStream })

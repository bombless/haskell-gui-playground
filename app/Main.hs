{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Tree (drawTree)
import Data.Word (Word8)

import Control.Monad (unless, when, filterM, replicateM)
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

offsetScale :: Int
offsetScale = 3

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
randomRInt (minVal, maxVal) = fst . randomR (minVal, maxVal) <$> newStdGen

-- 生成一个包含指定数量随机整数的列表
generateRandomList :: Int -> (Int, Int) -> IO [Int]
generateRandomList count range = replicateM count (randomRInt range)

generateNumbers :: IO [Int]
generateNumbers = generateRandomList 10 (0, 999)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  SDL.Font.initialize
  renderer <- createRenderer window (-1) defaultRenderer
  fonts <- cycleFonts
  numbers <- generateNumbers
  appLoop AppContext { ctxFirstTime = True
                     , ctxWindow = window
                     , ctxRenderer = renderer
                     , ctxNumbers = numbers
                     , ctxFonts = fonts
                     , ctxColors = colors
                     , ctxOffset = (0, 0)
                     , ctxLayoutOffset = 0
                     }
  destroyWindow window

colors :: [(V4 Word8, V4 Word8, V4 Word8)]
colors = cycle [
  (V4 173 216 230 255, V4 0 0 139 255, V4 255 255 255 255),
  (V4 144 238 144 255, V4 0 128 0 255, V4 240 230 140 255)]

data AppContext = AppContext { ctxFirstTime :: Bool
                             , ctxWindow :: Window
                             , ctxRenderer :: Renderer
                             , ctxNumbers :: [Int]
                             , ctxFonts :: [(FilePath, Font)]
                             , ctxColors :: [(V4 Word8, V4 Word8, V4 Word8)]
                             , ctxOffset :: (Int, Int)
                             , ctxLayoutOffset :: Int
                             }

appLoop :: AppContext -> IO ()
appLoop (AppContext {ctxFonts = []}) = undefined
appLoop (AppContext {ctxColors = []}) = undefined
appLoop ctx@AppContext { ctxRenderer = renderer, ctxFonts = (fontPath, font): otherFonts, ctxColors = colorConfig: otherColors } = do
  events <- pollEvents
  
  let eventKeyPressed code event =
        case eventPayload event of
          QuitEvent -> True
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == code
          _ -> False
      keyPressed code = any (eventKeyPressed code) events
  let eventAnyKeyPressed codes event =
        case eventPayload event of
          QuitEvent -> True
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            elem (keysymKeycode (keyboardEventKeysym keyboardEvent)) codes
          _ -> False
      anyKeyPressed code = any (eventAnyKeyPressed code) events
  let quitTriggered = keyPressed KeycodeQ
      tabPressed = keyPressed KeycodeTab
      leftPressed = keyPressed KeycodeLeft
      rightPressed = keyPressed KeycodeRight
      upPressed = keyPressed KeycodeUp
      downPressed = keyPressed KeycodeDown
      minusPressed = keyPressed KeycodeKPMinus
      plusPressed = keyPressed KeycodeKPPlus
      enterPressed = anyKeyPressed [KeycodeKPEnter, KeycodeReturn]
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

  drawTree (ctxFirstTime ctx) drawText (ctxNumbers ctx) (ctxOffset ctx) (ctxLayoutOffset ctx) renderer

  present renderer
  let fontStream = if tabPressed then otherFonts else (fontPath, font): otherFonts
  let colorStream = if enterPressed then otherColors else colorConfig: otherColors
  numbers <- if tabPressed
    then generateNumbers else return $ ctxNumbers ctx
  let (offsetX, offsetY) = ctxOffset ctx
  let newOffsetX = if leftPressed then offsetX - offsetScale else if rightPressed then offsetX + offsetScale else offsetX
  let newOffsetY = if upPressed then offsetY - offsetScale else if downPressed then offsetY + offsetScale else offsetY
  let layoutOffset = ctxLayoutOffset ctx
  let newLayoutOffset = if layoutOffset > -15 && minusPressed then layoutOffset - 1 else if plusPressed then layoutOffset + 1 else layoutOffset
  when (newLayoutOffset /= layoutOffset) $ printf "newLayoutOffset: %d\n" newLayoutOffset
  let nextContext = ctx { ctxNumbers = numbers
                        , ctxFirstTime = tabPressed
                        , ctxFonts = fontStream
                        , ctxColors = colorStream
                        , ctxOffset = (newOffsetX, newOffsetY)
                        , ctxLayoutOffset = newLayoutOffset
                        }
  unless quitTriggered (appLoop nextContext)

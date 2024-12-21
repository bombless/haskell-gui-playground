{-# LANGUAGE NamedFieldPuns #-}

module Tree (drawTree, generateParameters) where

import SDL
import Data.Text (pack, Text)
import Text.Printf (printf)

import Control.Monad (when)

import Tree.Definition
import qualified Tree.RedBlack

data Element a = VirtualLeft | VirtualRight | VirtualNode | VisibleNode a Bool Bool | VisibleLeft | VisibleRight
    deriving Show

heightOfDepth :: Int -> Int
heightOfDepth n = case n of
    2 -> 2
    3 -> 3
    _ -> if n <= 1 then 0 else 1 + 2 * heightOfDepth (n - 1)

leftmostSpaceOfDepth :: Int -> Int
leftmostSpaceOfDepth 2 = 2
leftmostSpaceOfDepth n = if n <= 1 then 0 else leftmostSpaceOfDepth (n-1) + heightOfDepth n + 1

normalSpaceOfDepth :: Int -> Int
normalSpaceOfDepth n = leftmostSpaceOfDepth (n+1) - 1

data Parameters = Parameters { maxWidth :: Int
                             , leafCount :: Int
                             , siblingSpace :: Int
                             , siblingGap :: Int
                             , barCount :: Int
                             , leftmostPadding :: Int
                             }
    deriving Show

generateNext_Parameter :: Parameters -> Maybe Parameters
generateNext_Parameter (Parameters { maxWidth
                                   , leafCount
                                   , siblingSpace
                                   , siblingGap
                                   , barCount
                                   , leftmostPadding
                                   })
    | leafCount > 1 =
        let spaceReduce = 2 * barCount + 2 in
        let nextLevelLeafCount = leafCount `div` 2 in
        let nextLevelMaxWidth = if nextLevelLeafCount > 1 then maxWidth - spaceReduce else 3 in
        let nextLevelSiblingSpace = if nextLevelLeafCount > 1 then (nextLevelMaxWidth - 3 * nextLevelLeafCount) `div` (nextLevelLeafCount - 1) else 0 in
        let nextLevelSiblingGap = nextLevelSiblingSpace in
        let nextLevelBarCount = if nextLevelLeafCount > 1 then (nextLevelSiblingSpace + 1) `div` 2 else 0 in
        let nextLevelLeftmostPadding = spaceReduce `div` 2 + leftmostPadding in
        Just (Parameters { maxWidth = nextLevelMaxWidth
                         , leafCount = nextLevelLeafCount
                         , siblingSpace = nextLevelSiblingSpace
                         , siblingGap = nextLevelSiblingGap
                         , barCount = nextLevelBarCount
                         , leftmostPadding = nextLevelLeftmostPadding
                         })
    | otherwise = Nothing

getLeafCount :: Int -> Int
getLeafCount 1 = 1
getLeafCount n = 2 * getLeafCount (n - 1)

generateParameters :: Int -> [Parameters]
generateParameters rootDepth =
    let leafCount = getLeafCount rootDepth in
    let maxWidth = (leafCount `div` 2 - 1) + 9 * (leafCount `div` 2) in
    let siblingSpace = 3 in
    let params = Parameters { maxWidth = maxWidth
                            , leafCount = leafCount
                            , siblingSpace = siblingSpace
                            , siblingGap = siblingSpace - 2
                            , barCount = (siblingSpace + 1) `div` 2
                            , leftmostPadding = 0
                            } in
    let aux params acc = case generateNext_Parameter params of
            Nothing -> acc
            Just params' -> aux params' (params':acc) in
    aux params [params]

getPaddingTree :: Tree a -> [Parameters] -> Bool -> Bool -> Tree (Int, a)
getPaddingTree t paramsList isLeft isLeftmost = case t of
    Leaf -> Leaf
    Node v l r ->
        Node (padding, v) (getPaddingTree l (tail paramsList) True isLeftmost) (getPaddingTree r (tail paramsList) False False) where
            Parameters { leftmostPadding, siblingSpace, siblingGap } = head paramsList
            padding
                | isLeftmost = leftmostPadding
                | isLeft = siblingGap
                | otherwise = siblingSpace


isVisualLeaf :: Tree (Int, Maybe a) -> Bool
isVisualLeaf Leaf = True
isVisualLeaf (Node (_, Just _) _ _) = False
isVisualLeaf (Node (_, Nothing) _ _) = True
                                    
generateFirstLine :: [Tree (Int, Maybe a)] -> [(Int, Element a)]
generateFirstLine [] = []
generateFirstLine (Node (p, Just c) l r:t) = (p, VisibleNode c (isVisualLeaf l) (isVisualLeaf r)) : generateFirstLine t
generateFirstLine (Node (p, Nothing) _ _:t) = (p, VirtualNode) : generateFirstLine t
generateFirstLine (Leaf:t) = generateFirstLine t

generateNextLine :: [(Int, Element a)] -> Bool -> [(Int, Element a)]
generateNextLine [] _ = []
generateNextLine ((n, c):t) first =
    let offset = if first then 1 else 2 in
    let next = generateNextLine t False in
    case c of
        VisibleLeft -> (n - offset, VisibleLeft) : next
        VirtualLeft -> (n - offset, VirtualLeft) : next
        VisibleRight -> (n + offset, VisibleRight) : next
        VirtualRight -> (n + offset, VirtualRight) : next
        VisibleNode _ leftIsLeaf rightIsLeaf -> (n, if leftIsLeaf then VirtualLeft else VisibleLeft) : (1, if rightIsLeaf then VirtualRight else VisibleRight) : next
        VirtualNode -> (n, VirtualLeft) : (1, VirtualRight) : next

generateLines :: [(Int, Element a)] -> Int -> [[(Int, Element a)]]
generateLines _ 0 = []
generateLines nodes n =
    let newLine = generateNextLine nodes True in
    newLine : generateLines newLine (n - 1)

asFullTree :: Tree a -> Int -> Tree (Maybe a)
asFullTree Leaf depth
    | depth > 0 = Node Nothing (asFullTree Leaf (depth - 1)) (asFullTree Leaf (depth - 1))
    | otherwise = Leaf
asFullTree (Node v l r) depth = Node (Just v) (asFullTree l (depth - 1)) (asFullTree r (depth - 1))

childrenOfNodes :: [Tree a] -> [Tree a]
childrenOfNodes [] = []
childrenOfNodes (Leaf:t) = childrenOfNodes t
childrenOfNodes (Node _ l r:t) = l : r : childrenOfNodes t

listOfNodes :: [Tree a] -> Int -> [[Tree a]]
listOfNodes _ 0 = []
listOfNodes lst depth =
    let children = childrenOfNodes lst in
    lst : listOfNodes children (depth - 1)

getDepth :: Tree a -> Int
getDepth Leaf = 0
getDepth (Node _ l r) = 1 + getDepth l `max` getDepth r

linesOfNodes :: (Int, [Tree (Int, Maybe a)]) -> [[(Int, Element a)]]
linesOfNodes (barCount, x) = let firstLine = generateFirstLine x in firstLine : generateLines firstLine barCount

transformBarCount :: [Parameters] -> [Int]
transformBarCount paramsList = transformBarCountHelper (tail paramsList) where
    transformBarCountHelper [] = [0]
    transformBarCountHelper (Parameters { barCount }:xs) = barCount : transformBarCountHelper xs

getLines :: Tree a -> [[(Int, Element a)]]
getLines t =
    let depth = getDepth t in
    let paramsList = generateParameters depth in
    let lists = listOfNodes [getPaddingTree (asFullTree t depth) paramsList True True] depth in
    let paramsAndNodesList = zip (transformBarCount paramsList) lists in
    concatMap linesOfNodes paramsAndNodesList

instance Printable Char where
    printNode a = putChar '|' >> putChar a >> putChar '|'

instance Printable Int where
    printNode = printf "%03d"

printLine :: (Printable a) => [(Int, Element a)] -> IO ()
printLine [] = putChar '\n'
printLine ((n, VisibleNode c _ _):t) = do
    putStr $ replicate n ' '
    printNode c
    printLine t
printLine ((n, VisibleLeft):t) = do
    putStr $ replicate n ' '
    putChar '/'
    printLine t
printLine ((n, VisibleRight):t) = do
    putStr $ replicate n ' '
    putChar '\\'
    printLine t
printLine ((n, VirtualNode):t) = do
    putStr $ replicate (n + 3) ' '
    printLine t
printLine ((n, _):t) = do
    putStr $ replicate (n + 1) ' '
    printLine t

printHelper :: (Printable a) => [[(Int, Element a)]] -> IO ()
printHelper = foldr ((>>) . printLine) (return ())

printTree :: (Printable a) => Tree a -> IO ()
printTree = printHelper . getLines

instance Insertable Char where
    insert v Leaf = Node v Leaf Leaf
    insert v (Node root left right)
        | v == root = Node root left right
        | v < root = Node root (insert v left) right
        | otherwise = Node root left (insert v right)

instance Insertable Int where
    insert v Leaf = Node v Leaf Leaf
    insert v (Node root left right)
        | v == root = Node root left right
        | v < root = Node root (insert v left) right
        | otherwise = Node root left (insert v right)

instance ToDrawingText Int where
    to_text x = Right (pack $ printf "%03d" x)

instance ToDrawingText Char where
    to_text x = Right (pack $ printf "%c" x)

drawNodes :: (ToDrawingText a) => Bool -> [[(Int, Element a)]] -> Int -> Int -> Int -> (Int, Int) -> (Text -> IO Surface) -> (Text -> IO Surface) -> Renderer -> IO ()
drawNodes _ [] _ _ _ _ _ _ _ = return ()
drawNodes firstTime ([]:t) initialX _ y (unitWidth, unitHeight) drawText drawRedText renderer = drawNodes firstTime t initialX initialX (y + unitHeight) (unitWidth, unitHeight) drawText drawRedText renderer
drawNodes firstTime (((n, VisibleNode content _ _):t):otherLines) initialX x y (unitWidth, unitHeight) drawText drawRedText renderer = do
    fillRect renderer $ Just $ Rectangle (P (V2 (fromIntegral $ x + n * unitWidth) (fromIntegral y))) (V2 (3 * fromIntegral unitWidth) (fromIntegral unitHeight))
    surface <- case to_text content of
        Left text -> drawRedText text
        Right text -> drawText text
    texture <- createTextureFromSurface renderer surface
    freeSurface surface
    dims <- surfaceDimensions surface
    when firstTime $ putStr "dims:" >> putStr (show dims) >> putChar '\n'
    let V2 textWidth textHeight = dims
    let offsetX = div (fromIntegral (3 * unitWidth) - fromIntegral textWidth) 2
    let offsetY = div (fromIntegral unitHeight - fromIntegral textHeight) 2
    let cX = fromIntegral $ x + n * unitWidth
    let cY = fromIntegral y
    let rectText = Rectangle (P (V2 (cX + offsetX) (cY + offsetY))) dims
    copy renderer texture Nothing $ Just rectText
    destroyTexture texture
    drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + unitWidth * 3) y (unitWidth, unitHeight) drawText drawRedText renderer
drawNodes firstTime (((n, VisibleLeft):t):otherLines) initialX x y (unitWidth, unitHeight) drawText drawRedText renderer = do
    let p1x = fromIntegral $ n * unitWidth + x + unitWidth
    let p1y = fromIntegral $ y
    let p2x = fromIntegral $ n * unitWidth + x
    let p2y = fromIntegral $ y + unitHeight
    drawLine renderer (P (V2 p1x p1y)) (P (V2 p2x p2y))
    drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText drawRedText renderer
drawNodes firstTime (((n, VisibleRight):t):otherLines) initialX x y (unitWidth, unitHeight) drawText drawRedText renderer = do
    let p1x = fromIntegral $ n * unitWidth + x
    let p1y = fromIntegral $ y
    let p2x = fromIntegral $ n * unitWidth + x + unitWidth
    let p2y = fromIntegral $ y + unitHeight
    drawLine renderer (P (V2 p1x p1y)) (P (V2 p2x p2y))
    drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText drawRedText renderer
drawNodes firstTime (((n, VirtualNode):t):otherLines) initialX x y (unitWidth, unitHeight) drawText drawRedText renderer = drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + 3 * unitWidth) y (unitWidth, unitHeight) drawText drawRedText renderer
drawNodes firstTime (((n, _):t):otherLines) initialX x y (unitWidth, unitHeight) drawText drawRedText renderer = drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText drawRedText renderer

drawTree :: Bool -> (Text -> IO Surface) -> (Text -> IO Surface) -> [Int] -> (Int, Int) -> Int -> Renderer -> IO ()
drawTree firstTime drawText drawRedText numbers (offsetX, offsetY) layoutOffset renderer = do
    when firstTime $ printf "drawTree layoutOffset: %d\n" layoutOffset
    let demo = Node 'D' (Node 'B' (Node 'A' Leaf Leaf) (Node 'C' Leaf Leaf)) (Node 'E' Leaf Leaf)
    when firstTime $ printTree demo
    let redBlackDemo = foldr Tree.RedBlack.insert Leaf numbers
    when firstTime $ print $ getLines redBlackDemo
    when firstTime $ printTree redBlackDemo
    let x = 100 + offsetX
    let y = 100 + offsetY
    drawNodes firstTime (getLines redBlackDemo) x x y (16 + layoutOffset, 32 + 2 * layoutOffset) drawText drawRedText renderer

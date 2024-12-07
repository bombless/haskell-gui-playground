module Tree (drawTree) where

import SDL
import Data.Text (pack, Text)
import Text.Printf (printf)

import Control.Monad (when)

data Tree a = Leaf | Node a (Tree a) (Tree a)
data Element a = VirtualLeft | VirtualRight | VirtualNode | VisibleNode a Bool Bool | VisibleLeft | VisibleRight

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

getPaddingTree :: Tree a -> Int -> Bool -> Bool -> Tree (Int, a)
getPaddingTree t depth isLeft isLeftmost = case t of
    Leaf -> Leaf
    Node v l r ->
        let padding = if isLeftmost
            then leftmostSpaceOfDepth depth
            else if isLeft && depth == 1 then 1 else normalSpaceOfDepth depth in
        Node (padding, v) (getPaddingTree l (depth - 1) True isLeftmost) (getPaddingTree r (depth-1) False False)

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
        VirtualNode -> (n - offset, VirtualLeft) : (1, VirtualRight) : next

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

linesOfNodes :: [Tree (Int, Maybe a)] -> [[(Int, Element a)]]
linesOfNodes x = case x of
    (item:_) ->
        let depth = getDepth item in
        let countDown = heightOfDepth depth in
        let firstLine = generateFirstLine x in
        firstLine : generateLines firstLine countDown
    _ -> []

getLines :: Tree a -> [[(Int, Element a)]]
getLines t =
    let depth = getDepth t in
    let lists = listOfNodes [getPaddingTree (asFullTree t depth) depth True True] depth in
    concat $ map linesOfNodes lists

class Printable a where
    printNode :: a -> IO ()

instance Printable Char where
    printNode a = putChar '|' >> putChar a >> putChar '|'

instance Printable Int where
    printNode a = printf "%03d" a

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
printLine ((n, _):t) = do
    putStr $ replicate (n + 1) ' '
    printLine t

printHelper :: (Printable a) => [[(Int, Element a)]] -> IO ()
printHelper [] = return ()
printHelper (h:t) = printLine h >> printHelper t

printTree :: (Printable a) => Tree a -> IO ()
printTree = printHelper . getLines

drawNodes :: Bool -> [[(Int, Element Int)]] -> Int -> Int -> (Int, Int) -> (Text -> IO Surface) -> Renderer -> IO ()
drawNodes _ [] _ _ _ _ _ = return ()
drawNodes firstTime ([]:t) _ y (unitWidth, unitHeight) drawText renderer = drawNodes firstTime t 0 (y + unitHeight) (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, VisibleNode content _ _):t):otherLines) x y (unitWidth, unitHeight) drawText renderer = do
    surface <- drawText $ pack $ printf "%03d" content
    texture <- createTextureFromSurface renderer surface
    dims <- surfaceDimensions surface
    let cX = fromIntegral $ x + n * unitWidth
    let cY = fromIntegral y
    let rectText = Rectangle (P (V2 cX cY)) dims
    copy renderer texture Nothing $ Just rectText
    drawNodes firstTime (t:otherLines) (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, VisibleLeft):t):otherLines) x y (unitWidth, unitHeight) drawText renderer = do
    let p1x = fromIntegral $ n * unitWidth + x + unitWidth
    let p1y = fromIntegral $ y
    let p2x = fromIntegral $ n * unitWidth + x
    let p2y = fromIntegral $ y + unitHeight
    if firstTime then do
        putStr "(x, y) = "
        putStr $ show (x, y)
        putStr ": "
        putStr "point "
        putStr $ show (p1x, p1y)
        putStr " to "
        putStr $ show (p2x, p2y)
        putChar '\n'
    else
        return ()
    drawLine renderer (P (V2 p1x p1y)) (P (V2 p2x p2y))
    drawNodes firstTime (t:otherLines) (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, VisibleRight):t):otherLines) x y (unitWidth, unitHeight) drawText renderer = do
    let p1x = fromIntegral $ n * unitWidth + x
    let p1y = fromIntegral $ y
    let p2x = fromIntegral $ n * unitWidth + x + unitWidth
    let p2y = fromIntegral $ y + unitHeight
    if firstTime then do
        print "point "
        print (p1x, p1y)
        putStr " to "
        print (p2x, p2y)
        putChar '\n'
    else
        return ()
    drawLine renderer (P (V2 p1x p1y)) (P (V2 p2x p2y))
    drawNodes firstTime (t:otherLines) (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, _):t):otherLines) x y (unitWidth, unitHeight) drawText renderer = drawNodes firstTime (t:otherLines) (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText renderer

drawTree :: Bool -> (Text -> IO Surface) -> Renderer -> IO ()
drawTree firstTime drawText renderer = do
    let demo = Node 2 (Node 1 Leaf Leaf) Leaf
    when firstTime $ printTree demo
    drawNodes firstTime (getLines demo) 0 0 (32, 32) drawText renderer
    -- surface <- drawText $ pack $ printf "Hello, world!"
    -- texture <- createTextureFromSurface renderer surface
    -- let rect = Just $ Rectangle (P (V2 0 96)) (V2 30 192)
    -- copy renderer texture rect Nothing
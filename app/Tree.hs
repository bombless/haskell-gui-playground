module Tree (drawTree) where

import SDL
import Data.Text (pack, Text)
import Text.Printf (printf)

import Control.Monad (when)

import Tree.Definition
import qualified Tree.RedBlack

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
        Node (padding, v) (getPaddingTree l (depth - 1) True isLeftmost) (getPaddingTree r (depth-1) False False) where
            padding
                | isLeftmost = leftmostSpaceOfDepth depth
                | isLeft && depth == 1 = 1
                | otherwise = normalSpaceOfDepth depth


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
    concatMap linesOfNodes lists

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

instance ToDrawingText Int where
    to_text x = pack $ printf "%03d" x

instance ToDrawingText Char where
    to_text x = pack $ printf "%c" x

drawNodes :: (ToDrawingText a) => Bool -> [[(Int, Element a)]] -> Int -> Int -> Int -> (Int, Int) -> (Text -> IO Surface) -> Renderer -> IO ()
drawNodes _ [] _ _ _ _ _ _ = return ()
drawNodes firstTime ([]:t) initialX _ y (unitWidth, unitHeight) drawText renderer = drawNodes firstTime t initialX initialX (y + unitHeight) (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, VisibleNode content _ _):t):otherLines) initialX x y (unitWidth, unitHeight) drawText renderer = do
    fillRect renderer $ Just $ Rectangle (P (V2 (fromIntegral $ x + n * unitWidth) (fromIntegral y))) (V2 (3 * fromIntegral unitWidth) (fromIntegral unitHeight))
    surface <- drawText $ to_text content
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
    drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + unitWidth * 3) y (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, VisibleLeft):t):otherLines) initialX x y (unitWidth, unitHeight) drawText renderer = do
    let p1x = fromIntegral $ n * unitWidth + x + unitWidth
    let p1y = fromIntegral $ y
    let p2x = fromIntegral $ n * unitWidth + x
    let p2y = fromIntegral $ y + unitHeight
    when firstTime
        $ do
        putStr "#VisibleLeft# (x, y) = "
        putStr $ show (x, y)
        putStr ": "
        putStr "point "
        putStr $ show (p1x, p1y)
        putStr " to "
        putStr $ show (p2x, p2y)
        putChar '\n'
    drawLine renderer (P (V2 p1x p1y)) (P (V2 p2x p2y))
    drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, VisibleRight):t):otherLines) initialX x y (unitWidth, unitHeight) drawText renderer = do
    let p1x = fromIntegral $ n * unitWidth + x
    let p1y = fromIntegral $ y
    let p2x = fromIntegral $ n * unitWidth + x + unitWidth
    let p2y = fromIntegral $ y + unitHeight
    when firstTime
        $ do
        putStr "#VisibleRight# (x, y) = "
        putStr $ show (x, y)
        putStr ": "
        putStr "point "
        putStr $ show (p1x, p1y)
        putStr " to "
        putStr $ show (p2x, p2y)
        putChar '\n'
    drawLine renderer (P (V2 p1x p1y)) (P (V2 p2x p2y))
    drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, VirtualNode):t):otherLines) initialX x y (unitWidth, unitHeight) drawText renderer = drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + 3 * unitWidth) y (unitWidth, unitHeight) drawText renderer
drawNodes firstTime (((n, _):t):otherLines) initialX x y (unitWidth, unitHeight) drawText renderer = drawNodes firstTime (t:otherLines) initialX (x + n * unitWidth + unitWidth) y (unitWidth, unitHeight) drawText renderer

drawTree :: Bool -> (Text -> IO Surface) -> Renderer -> IO ()
drawTree firstTime drawText renderer = do
    -- let demo = Node 'D' (Node 'B' (Node 'A' Leaf Leaf) (Node 'C' Leaf Leaf)) (Node 'E' Leaf Leaf)
    let demo = insert 'D' $ insert 'A' $ insert 'F' $ insert 'H' $ insert 'E' Leaf
    when firstTime $ printTree demo
    let redBlackDemo = foldr Tree.RedBlack.insert Leaf ['E', 'H', 'F', 'A', 'D']
    when firstTime $ printTree redBlackDemo
    drawNodes firstTime (getLines demo) 100 100 100 (16, 32) drawText renderer
{-# LANGUAGE FlexibleInstances #-}

module Tree.RedBlack (Tree.RedBlack.insert) where

import Tree.Definition
import Data.Text (pack)
import Text.Printf (printf)

data Color = Red | Black
    deriving Show

newtype RedBlackInt = RedBlackInt (Color, Int)
newtype RedBlackChar = RedBlackChar (Color, Char)

place :: a -> a -> a -> Tree (Color, a) -> Tree (Color, a) -> Tree (Color, a) -> Tree (Color, a) -> Tree (Color, a)
place x y z a b c d = Node (Red, y) (Node (Black, x) a b) (Node (Black, z) c d)

balance :: ((Color, a), Tree (Color, a), Tree (Color, a)) -> Tree (Color, a)
balance ((Black, z), Node (Red, y) (Node (Red, x) a b) c, d) = place x y z a b c d
balance ((Black, z), Node (Red, x) a (Node (Red, y) b c), d) = place x y z a b c d
balance ((Black, x), a, Node (Red, y) b (Node (Red, z) c d)) = place x y z a b c d
balance ((Black, x), a, Node (Red, z) (Node (Red, y) b c) d) = place x y z a b c d
balance (n, l, r) = Node n l r

blackRoot :: Tree (Color, a) -> Tree (Color, a)
blackRoot Leaf = Leaf
blackRoot (Node (_, v) l r) = Node (Black, v) l r

insertAux :: (Ord a) => a -> Tree (Color, a) -> Tree (Color, a)
insertAux v Leaf = Node (Red, v) Leaf Leaf
insertAux v (Node (c, nv) lt rt)
    | v == nv = Node (c, nv) lt rt
    | v < nv = balance ((c, nv), insertAux v lt, rt)
    | otherwise = balance ((c, nv), lt, insertAux v rt)

insert :: (Ord a) => a -> Tree (Color, a) -> Tree (Color, a)
insert v t = blackRoot $ insertAux v t

instance ToDrawingText RedBlackInt where
    to_text (RedBlackInt (Red, x)) = Left (pack $ printf "%03d" x)
    to_text (RedBlackInt (Black, x)) = Right (pack $ printf "%03d" x)

instance ToDrawingText (Color, Int) where
    to_text (Red, x) = Left (pack $ printf "%03d" x)
    to_text (Black, x) = Right (pack $ printf "%03d" x)

instance ToDrawingText (Color, Char) where
    to_text (Red, x) = Left (pack $ printf "%c" x)
    to_text (Black, x) = Right (pack $ printf "%c" x)

redText :: String
redText = "\027[31m"

resetColor :: String
resetColor = "\027[0m"

instance Printable (Color, Int) where
    printNode (Red, x) = printf "%s%03d%s" redText x resetColor
    printNode (_, x) = printf "%03d" x

instance Printable (Color, Char) where
    printNode (Red, x) = putChar '|' >> putStr redText >> putChar x >> putStr resetColor >> putChar '|'
    printNode (_, x) = putChar '|' >> putChar x >> putChar '|'

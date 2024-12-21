module Tree.Definition where

import Data.Text (Text)

data Tree a = Leaf | Node a (Tree a) (Tree a)


class Printable a where
    printNode :: a -> IO ()


class ToDrawingText a where
    to_text :: a -> Either Text Text


class Insertable a where
    insert :: a -> Tree a -> Tree a

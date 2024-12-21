module Tree.Definition where

import Data.Text (Text, pack)
import Text.Printf (printf)

data Tree a = Leaf | Node a (Tree a) (Tree a)


class Printable a where
    printNode :: a -> IO ()

instance Printable Char where
    printNode a = putChar '|' >> putChar a >> putChar '|'

instance Printable Int where
    printNode = printf "%03d"


class ToDrawingText a where
    to_text :: a -> Either Text Text

instance ToDrawingText Int where
    to_text x = Right (pack $ printf "%03d" x)

instance ToDrawingText Char where
    to_text x = Right (pack $ printf "%c" x)
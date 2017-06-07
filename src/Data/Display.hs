module Data.Display (ContentClass(..), Displayable(..), TextOut(..),
    displayStr) where

data ContentClass = Variable | Units | Numeric | Symbol | Name |
    ErrorMsg | Prompt
type TextOut = [(ContentClass, String)]

class Displayable a where
    display :: a -> TextOut

displayStr :: Displayable a => a -> String
displayStr = concat . map snd . display


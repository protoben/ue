module Data.Display (ContentClass(..), Displayable(..), displayStr) where

data ContentClass = Variable | Units | Numeric | Symbol | Name

class Displayable a where
    display :: a -> [(ContentClass, String)]

displayStr :: Displayable a => a -> String
displayStr = concat . map snd . display

module Hero.UI where

import Control.Monad(when)
import Data.Maybe(fromJust)

import Data.Array.IArray(assocs)
import UI.NCurses

import Hero.Map


-- | Update the NCurses UI by displaying the whole map.
displayMap :: Map -> Update ()
displayMap m = do moveCursor 0 0
                  go 0 . assocs $ m
  where
    go :: Integer -> [(Point, Tile)] -> Update ()
    go _ []          = return ()
    go y ((pt,t):xs) = do when (y /= y') $
                            moveCursor y' 0
                          drawString . show $ t
                          go y' xs
      where
        y' = fst pt
      

nextChar :: Window -> Curses (Char)
nextChar w = do ev <- getEvent w Nothing
                case (fromJust ev) of EventCharacter c -> return c
                                      _                -> nextChar w

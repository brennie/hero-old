module Hero.Game where

import Data.Array.IArray((!))
import Control.Monad(when)
import UI.NCurses

import Hero.Map
import Hero.UI(displayMap, nextChar)


-- | Initialize the UI and start the game.
runGame :: IO ()
runGame = do case m of
               Nothing -> putStrLn "Could not load map"
               Just m' -> runCurses $ do setEcho False
                                         setCursorMode CursorInvisible
                                         w <- defaultWindow
                                         gameLoop w m' (1, 1)
  where
    m = readMap . unlines $ ["################################"
                            ,"#....###..##....##..v..###..#..#"
                            ,"#...............##..#..###..#..#"
                            ,"#....###..##....##.....###....##"
                            ,"#....###..##....##.........##.##"
                            ,"########..###################..#"
                            ,"#...............#####.....####.#"
                            ,"#......................##......#"
                            ,"################################"]


-- | Run the game loop.
gameLoop :: Window -> Map -> Point -> Curses ()
gameLoop w m p = do updateWindow w $ do displayMap m
                                        uncurry moveCursor $ p
                                        drawString "@"
                    
                    render

                    c <- nextChar w

                    when (c /= 'q') $ do
                      case readDirection c of
                        Nothing -> gameLoop w m p
                        Just d  -> do let tile = m ! p'
                                      if tile == StairsDown then win
                                      else gameLoop w m p'
                          where
                            moved = move p d
                            p' = if walkable $ m ! moved then moved else p
  where
    win :: Curses ()
    win = do updateWindow w $ do moveCursor 10 0
                                 drawString "You have won! Press any key to continue . . ."

             render

             _ <- nextChar w
             
             return ()

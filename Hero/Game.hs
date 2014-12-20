module Hero.Game where

import Data.Array.IArray((!))
import Control.Monad(when)
import UI.NCurses

import Hero.Character
import Hero.Map
import Hero.UI(displayMap, nextChar)


data GameState = GameState { level :: Map, char :: Character }


-- | Initialize the UI and start the game.
runGame :: String ->  IO ()
runGame n = do case m of
                 Nothing -> putStrLn "Could not load map"
                 Just m' -> runCurses $ do setEcho False
                                           setCursorMode CursorInvisible
                                           w <- defaultWindow
                                           gameLoop w (GameState m' (Character n (1, 1) 2))
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
gameLoop :: Window -> GameState -> Curses ()
gameLoop w st = do updateWindow w $ do displayMap . level $ st
                                       uncurry moveCursor . position . char $ st
                                       drawString "@"
                        
                   render

                   c <- nextChar w

                   when (c /= 'q') $ do
                     case readDirection c of
                       Nothing -> gameLoop w st
                       Just d  -> gameLoop w st'
                         where
                           p' = move (position . char $ st) d
                           st' = if walkable $ (level st) ! p' then st { char = (char st) { position = p' } }
                                                               else st

                           
  where
    win :: Curses ()
    win = do updateWindow w $ do moveCursor 10 0
                                 drawString "You have won! Press any key to continue . . ."

             render

             _ <- nextChar w
             
             return ()

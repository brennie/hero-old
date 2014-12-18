module Hero.Game where


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
                            ,"#....###..##....##.....###..#..#"
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
                    case c of
                      'q' -> return ()
                      _   -> gameLoop w m p

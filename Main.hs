module Main where


import Hero.Game(runGame)


main :: IO ()
main = do putStr "Enter your name: "
          name <- getLine
          runGame name

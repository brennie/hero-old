module Hero.Character where


import Hero.Map


data Character = Character { name     :: String
                           , position :: Point
                           , sight    :: Int }

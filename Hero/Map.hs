module Hero.Map where


import Data.Char(toLower)
import Data.Array.IArray(Array, listArray)


-- | The different types of map tiles.
data Tile = Floor | Wall | StairsDown deriving Eq


-- | Convert a Tile to a String
--
-- We do not convert to Char because NCurses only handles drawing strings.
instance Show Tile where
    show Floor      = "."
    show Wall       = "#"
    show StairsDown = "v"


-- | Determine if a character can step on a Tile.
walkable :: Tile -> Bool
walkable Floor      = True
walkable Wall       = False
walkable StairsDown = True


-- | Attempt to interpret a Char as a Tile.
--
-- Invalid characters are interpreted as Nothing.
readTile :: Char -> Maybe Tile
readTile '.' = Just Floor
readTile '#' = Just Wall
readTile 'v' = Just StairsDown
readTile  _  = Nothing


-- | A point in the map.
--
-- We use (Integer, Integer) instead of (Int, Int) so we can avoid type
-- conversions when dealing with NCurses functions.
--
-- Points are of the form (Y, X) because they are indexes into a two
-- dimensional array and into screen coordinates.
type Point = (Integer, Integer)


-- | A Map, which is a two dimensional array of tiles.
type Map = Array Point Tile


-- | Attempt to interpret a String as a Map.
--
-- If any single character in the string cannot be interpreted successfully,
-- then the interpretation of the entire string fails.
readMap :: String -> Maybe Map
readMap s
    | null rows       = Nothing -- We do not want an empty Map.
    | w * h == 0      = Nothing -- Same as above.
    | not isRectangle = Nothing -- The resulting array must not be jagged.
    | otherwise       = do tiles <- sequence2 . (map . map) readTile $ rows
                           return . listArray dimensions . concat $ tiles
    where
      rows = lines s

      w = length . head $ rows
      h = length $ rows

      -- Determine if the list of lists is a rectangle, i.e., where the
      -- sub-lists all have the same length.
      isRectangle = and . map ((== w) . length) $ tail rows

      yMax = toInteger $ h - 1 -- 0-based height of the array
      xMax = toInteger $ w - 1 -- 0-based width of the array

      dimensions = ((0, 0), (yMax, xMax))

      -- | Sequence a list of lists.
      sequence2 :: (Monad m) => [[m a]] -> m [[a]]
      sequence2 = sequence . map sequence


-- | A cardinal direction.
data Direction = North | South | East | West


-- | Try to interpret a Char as a direction, using the WASD mapping.
readDirection :: Char -> Maybe Direction
readDirection = go . toLower
  where
    go 'w' = Just North
    go 'a' = Just West
    go 's' = Just South
    go 'd' = Just East
    go  _  = Nothing


-- | Move from a point in a given direction.
move :: Point -> Direction -> Point
move (y, x) North = (y - 1, x)
move (y, x) South = (y + 1, x)
move (y, x) East  = (y, x + 1)
move (y, x) West  = (y, x - 1)

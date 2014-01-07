module Main where

import Graphics.Gloss
import Snowflakes
import System.Random
import System.Environment

drawSnowflake :: Snowflake -> Point -> Picture
drawSnowflake snowy startPos =
  let expanded = expandPattern snowy
      segmentLen = 1
      folding (sx, sy) (Segment angle) =
        let nx = fromInteger $ round $ sx + segmentLen * (cos angle)
            ny = fromInteger $ round $ sy + segmentLen * (sin angle)
        in (nx, ny)
      points = scanl folding startPos expanded
      lines = [Line [s, e] | (s, e) <- zip points (tail points)]
  in Color white $ Pictures lines

main = do
  args <- getArgs
  let factor = (read (args !! 0)) :: Int
  let size = (read (args !! 1)) :: Int
  gen <- getStdGen
  let snowy = randomSnowflake factor size gen
  let snowflakePic = drawSnowflake snowy (0, 0)
  display (InWindow "Snowflakes" (2000, 2000) (10, 10)) black snowflakePic

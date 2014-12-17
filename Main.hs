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
-- program takes two arguments
-- first one is the count of symmetry axises (best results are with value 8)
-- second one is the segment count in the snowflake generator (value about 10k is pretty good)
  let factor = 8
  let size = 15000
  gen <- getStdGen
  let snowy = randomSnowflake factor size gen
  let snowflakePic = drawSnowflake snowy (0, 0)
  display (InWindow "Snowflakes" (1000, 1000) (10, 10)) black snowflakePic

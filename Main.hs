module Main where

import Graphics.Gloss
import Snowflakes
import System.Random

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

printCalculations :: Snowflake -> Point -> IO ()
printCalculations snowy startPos = do
  let expanded = expandPattern snowy
      segmentLen = 1
      folding (sx, sy) (Segment angle) =
        let nx = fromInteger $ round $ sx + segmentLen * (cos angle)
            ny = fromInteger $ round $ sy + segmentLen * (sin angle)
        in (nx, ny)
      points = scanl folding startPos expanded
      lines = [Line [s, e] | (s, e) <- zip points (tail points)]
  putStrLn $ show expanded
  putStrLn $ show lines

main = do
  gen <- getStdGen
  let snowy = randomSnowflake 8 200 gen
  --putStrLn $ show snowy
  --printCalculations snowy (0, 0)
  let snowflakePic = drawSnowflake snowy (0, 0)
  display (InWindow "Nice Window" (500, 500) (10, 10)) black snowflakePic

module Main where

import Graphics.Gloss
import Snowflakes
import System.Random

glossifySnowflake :: Snowflake -> Point -> Picture
glossifySnowflake snowy startPos =
  let expanded = expandPattern snowy
      segmentLen = 10
      folding (sx, sy) (Segment angle) =
        let nx = sx + segmentLen * (cos angle)
            ny = sy + segmentLen * (sin angle)
        in (nx, ny)
      points = scanl folding startPos expanded
      lines = [Line [s, e] | (s, e) <- zip points (tail points)]
  in Color white $ Pictures lines


printCalculations :: Snowflake -> Point -> IO ()
printCalculations snowy startPos = do
  let expanded = expandPattern snowy
      segmentLen = 10
      folding (sx, sy) (Segment angle) =
        let nx = fromInteger $ round $ sx + segmentLen * (cos angle)
            ny = fromInteger $ round $ sy + segmentLen * (sin angle)
        in (nx, ny)
      points = scanl folding startPos expanded
      lines = [Line [s, e] | (s, e) <- zip points (tail points)]
  putStrLn (show lines)

main = do
  gen <- getStdGen
  let (snowy, _) = randomSnowflake 4 2 gen
  putStrLn $ show snowy
  printCalculations snowy (400, 400)
  let snowflakePic = glossifySnowflake snowy (400, 400)
  display (InWindow "Nice Window" (800, 800) (3, 3)) black snowflakePic

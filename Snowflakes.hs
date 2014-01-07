module Snowflakes where

import System.Random

data Segment = Segment Float
             deriving (Show)
type Pattern = [Segment]

data Snowflake = Snowflake {
  factor :: Int, -- factor is the count of segments in pattern
  pattern :: Pattern
} deriving (Show)

randomPattern :: (RandomGen g) => Int -> Int -> g -> Pattern
randomPattern factor size gen =
  let zeroToNs = take size $ randomRs (0, factor - 1) gen
      angles = [ (pi :: Float) * (fromIntegral n) / (fromIntegral factor)
                 | n <- zeroToNs]
      segments = map Segment angles
  in segments 

randomSnowflake :: (RandomGen g) => Int -> Int -> g -> Snowflake
randomSnowflake factor size gen =
  let pattern = randomPattern factor size gen
      snowflake = Snowflake { pattern = pattern, factor = factor }
  in snowflake

-- expand pattern to full snowflakeexpandPattern :: Snowflake -> [Segment]
expandPattern snowy =
  let fac = factor snowy
      pat = pattern snowy
  in [newSegment |
        part <- [1..fac],
        segment <- pat,
        let Segment angle = segment,
        let q = 2 * pi * ((fromIntegral part) / (fromIntegral fac)),
        let newSegment = Segment (q + angle)]

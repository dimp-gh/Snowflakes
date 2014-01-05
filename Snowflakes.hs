module Snowflakes where

import System.Random

data Segment = Segment Float deriving (Show)
type Pattern = [Segment]

data Snowflake = Snowflake {
  factor :: Integer, -- factor is the count of segments in pattern
  pattern :: Pattern
} deriving (Show)

randomPattern :: (RandomGen g) => Integer -> Integer -> g -> (Pattern, g)
randomPattern factor size gen = randomPattern' [] factor size gen

randomPattern' :: (RandomGen g) => Pattern -> Integer -> Integer -> g -> (Pattern, g)
randomPattern' acc factor 0 gen = (acc, gen)
randomPattern' acc factor size gen =
  let (n, newGen) = randomR (0, factor - 1) gen
      angle = (pi :: Float) * 2 * ((fromInteger n) / (fromInteger factor))
      segment = Segment angle
  in randomPattern' (segment : acc) factor (size - 1) newGen

randomSnowflake :: (RandomGen g) => Integer -> Integer -> g -> (Snowflake, g)
randomSnowflake factor size gen =
  let (pattern, newGen) = randomPattern factor size gen
      snowflake = Snowflake { pattern = pattern, factor = factor }
  in (snowflake, newGen)

-- expand pattern to full snowflake
expandPattern :: Snowflake -> [Segment]
expandPattern snowy =
  let fac = factor snowy
      pat = pattern snowy
  in [newSegment |
        i <- [1..fac],
        segment <- pat,
        let Segment angle = segment,
        let newSegment = Segment (angle * (fromIntegral i))]

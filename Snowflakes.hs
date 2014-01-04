module Snowflakes where

import System.Random

data Segment = Segment Float
type Pattern = [Segment]

data Snowflake = Snowflake {
  pattern :: Pattern,
  factor :: Integer
}

randomPattern :: (RandomGen g) => Integer -> g -> (Pattern, g)
randomPattern factor gen = undefined

snowflakeFromPattern :: Pattern -> Integer -> Snowflake
snowflakeFromPattern pattern factor = undefined

randomSnowflake :: (RandomGen g) => g -> (Snowflake, g)
randomSnowflake gen = undefined

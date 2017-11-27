-- | A Lib module.
module Lib
    ( tossDice
    ) where

      import System.Random
      import System.Environment

      tossDice :: RandomGen g => g -> [Char]
      tossDice g = take 1 (randomRs ('1', '6') g)

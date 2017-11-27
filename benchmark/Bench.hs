module Main (main) where

import              Criterion.Main (bench, bgroup, defaultMain, nf)
import              Lib

main :: IO ()
main = defaultMain
    [ bgroup "Lib" [ bench "AnnA" $ nf palindrom ("Anna" :: String)
                    , bench "ananananananananana" $ nf palindrom ("ananananananananana" :: String)
                    , bench "ananananananananananananananananananananananananananana" $ nf palindrom ("ananananananananananananananananananananananananananana" :: String)
                    , bench "anananananananananananananananananananananananananananananananananananananananananananananananananananananana" $ nf palindrom ("anananananananananananananananananananananananananananananananananananananananananananananananananananananana" :: String)
                    ]
    ]
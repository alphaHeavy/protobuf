{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data
import Nested

main :: IO ()
main = defaultMain [
  bgroup "burst" [
     bgroup "encoding" [
        bench "1" $ nf enc (burst 1),
        bench "10" $ nf enc (burst 10),
        bench "100" $ nf enc (burst 100),
        bench "1000" $ nf enc (burst 1000)
        ],
     bgroup "decoding" [
        bench "1" $ nf decBurst (encodeBurst 1),
        bench "10" $ nf decBurst (encodeBurst 10),
        bench "100" $ nf decBurst (encodeBurst 100),
        bench "1000" $ nf decBurst (encodeBurst 1000)
        ]
  ],
  bgroup "nested" [
     bgroup "encoding" [
        bench "a1" $ nf enc (nested 1 1),
        bench "a10" $ nf enc (nested 1 10),
        bench "a100" $ nf enc (nested 1 100),
        bench "a1000" $ nf enc (nested 1 1000),
        bench "b10" $ nf enc (nested 10 1),
        bench "b100" $ nf enc (nested 100 1),
        bench "b1000" $ nf enc (nested 1000 1)
     ],
     bgroup "decoding" [
        bench "a1" $ nf decNested (enc $ nested 1 1),
        bench "a10" $ nf decNested (enc $ nested 1 10),
        bench "a100" $ nf decNested (enc $ nested 1 100),
        bench "a1000" $ nf decNested (enc $ nested 1 1000),
        bench "b10" $ nf decNested (enc $ nested 10 1),
        bench "b100" $ nf decNested (enc $ nested 100 1),
        bench "b1000" $ nf decNested (enc $ nested 1000 1)
        ]
     ]
  ]

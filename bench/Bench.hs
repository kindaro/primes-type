module Main where

import Criterion
import Criterion.Main

import Data.Numbers.Primes.Type

n = 2 ^ 16
p :: Prime Int
p = getPrime n

main = defaultMain
        [ env (return p) benchPrimes
        ]

benchPrimes p = bgroup "Some benchmarks"
    [ bench "getValue"      $ nf getValue p
    , bench "getIndex"      $ nf getIndex p
    , bench "deconstruct"   $ nf deconstruct p
    , bench "primeIndex"    $ nf primeIndex (getValue p)
    , bench "getPrime"      $ nf (getPrime :: Int -> Prime Int) n
    , bench "maybePrime"    $ nf maybePrime (getValue p)
    , bench "indexedPrimes" $ nf (flip take (indexedPrimes :: [Prime Int])) n
    ]

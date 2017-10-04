{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Numbers.Primes.Type
    ( Prime
    , getValue
    , getIndex
    , deconstruct
    , primeIndex
    , getPrime
    , maybePrime
    ) where

import Data.List (elemIndex)
import Data.Numbers.Primes

-- | An abstract type for primes.
--
--   It will only ever hold a valid prime, together with its zero-based index.
data Prime int = Prime { _value :: !int, _index :: !Int } deriving Show

-- | Given a Prime, give back its value.
getValue :: Prime int -> int
getValue = _value

-- | Given a Prime, give back its index.
getIndex :: Prime int -> Int
getIndex = _index

-- | Given a Prime, give back its value and index as a tuple.
deconstruct :: Prime int -> (int, Int)
deconstruct p = (_value p, _index p)

instance Integral int => Enum (Prime int) where
    toEnum = getPrime
    fromEnum = getIndex

instance Eq (Prime int) where
    x == y = getIndex x == getIndex y

instance Ord (Prime int) where
    x `compare` y = getIndex x `compare` getIndex y

-- | If a given number is prime, give its index.
primeIndex :: Integral n => n -> Maybe Int
primeIndex x | isPrime x = elemIndex x primes
             | otherwise = Nothing

-- | Give n-th prime.
getPrime :: Integral int => Int -> Prime int
getPrime n = Prime (primes !! n) n

-- | If a given number is prime, give it back wrapped as such.
maybePrime :: (Integral int) => int -> Maybe (Prime int)
maybePrime x = Prime x <$> primeIndex x

-- | A list of indexed primes.
indexedPrimes :: Integral int => [Prime int]
indexedPrimes = getPrime <$> [0,1..]

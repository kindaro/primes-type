{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Numbers.Primes.Type
    ( Prime
    , getValue
    , getIndex
    , primeIndex
    , getPrime
    , maybePrime
    ) where

import Data.List (elemIndex)
import Data.Numbers.Primes

data Prime int = Prime { _value :: int, _index :: !Int } deriving Show

-- | Given a Prime, give back its value.
getValue :: Prime int -> int
getValue = _value

-- | Given a Prime, give back its index.
getIndex :: Prime int -> Int
getIndex = _index

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

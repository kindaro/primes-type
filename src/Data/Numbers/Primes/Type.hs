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
import Control.Applicative ((<$>)) -- This is a compatibility measure.

data Prime int = Prime { getValue :: int, getIndex :: Int } deriving Show

instance Integral int => Enum (Prime int) where
    toEnum = getPrime
    fromEnum = getIndex

instance Eq (Prime int) where
    x == y = getIndex x == getIndex y

instance Ord (Prime int) where
    x `compare` y = getIndex x `compare` getIndex y

-- | If a given number is prime, give its index.
primeIndex :: (Integral n, Integral i) => n -> Maybe i
primeIndex x | isPrime x = fromIntegral <$> elemIndex x primes
             | otherwise = Nothing

-- | Give n-th prime.
getPrime :: (Integral n, Integral int) => n -> Prime int
getPrime n = Prime (primes !! fromIntegral n) (fromIntegral n)

-- | If a given number is prime, give it back wrapped as such.
maybePrime :: (Integral n, Integral int) => n -> Maybe (Prime int)
maybePrime x | isPrime x = Prime (fromIntegral x) <$> primeIndex x
             | otherwise = Nothing

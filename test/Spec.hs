{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Data.Numbers.Primes
import Data.Numbers.Primes.Type
import Data.Word

instance (Arbitrary a, Integral a) => Arbitrary (Prime a) where
    arbitrary = getPrime <$> arbitrarySizedNatural

main = htfMain htf_thisModulesTests

-- | Some basic sanity checks.

test_prime = do
    assertEqual (Just $ getPrime 0) (maybePrime 2)
    assertEqual (Just $ getPrime 5) (maybePrime 13)
    assertEqual (primeIndex 2) (Just 0)
    assertEqual (primeIndex 13) (Just 5)
    assertEqual (maybePrime 4) (Nothing)

-- | These two properties show that Prime and its value are
--   isomorphic if and only if the value is prime.
--   (n in N but not in P are mapped to Nothing with no inverse.)

-- prop_maybePrime :: Integral int => int -> Bool -- ^ This is too general to suit a prop.
prop_maybePrime_Int = withQCArgs (\prop -> prop { maxSize = 1024 } ) prop_maybePrime_Int'
  where prop_maybePrime_Int' :: Int -> Bool
        prop_maybePrime_Int' x = r == Just x || r == Nothing
          where r :: Maybe Int
                r = getValue <$> maybePrime x

prop_maybePrime_Integer = withQCArgs (\prop -> prop { maxSize = 1024 } ) prop_maybePrime_Integer'
  where prop_maybePrime_Integer' :: Integer -> Bool
        prop_maybePrime_Integer' x = r == Just x || r == Nothing
          where r :: Maybe Integer
                r = getValue <$> maybePrime x

prop_maybePrime_Word = withQCArgs (\prop -> prop { maxSize = 5 } ) prop_maybePrime_Word'
  where prop_maybePrime_Word' :: Word8 -> Bool
        prop_maybePrime_Word' x = r == Just x || r == Nothing
          where r :: Maybe Word8
                r = getValue <$> maybePrime x

prop_getValue_Int = withQCArgs (\prop -> prop { maxSize = 1024 } ) prop_getValue_Int'
  where
    prop_getValue_Int' :: Int -> Bool
    prop_getValue_Int' n = (maybePrime . getValue $ p) == Just p
      where
        p :: Prime Int
        p | n < 0 = getPrime . abs $ n -- I know it's not super bright.
          | otherwise = getPrime n

-- | These two properties show that Prime and its index are isomorphic.

prop_primeIndex :: Int -> Bool
prop_primeIndex n = m == (getIndex <$> getPrime) m
  where m = abs n

prop_getIndex :: Int -> Bool
prop_getIndex n = getPrime m == (getPrime . getIndex . getPrime) m
  where m = abs n

-- | This property verifies the correctness of `deconstruct`.

prop_deconstruct i = deconstruct p == (getValue p, getIndex p)
  where
    p = getPrime (abs i)

-- | These two properties show that the enumeration associated with Prime is isomorphic.

prop_fromEnum :: Prime Int -> Bool
prop_fromEnum p = p == (toEnum . fromEnum) p

prop_toEnum :: Int -> Bool
prop_toEnum n' = n == (fromEnum . (toEnum :: Int -> Prime Int)) n
  where
    n = abs n'

-- | Equality & ordering of primes is the same as the respective relations on their indices.

prop_Eq = withQCArgs (\prop -> prop { maxSize = 256 } ) prop_Eq'
  where
    prop_Eq' :: (Int, Int) -> Bool
    prop_Eq' (i', j') = (i == j) == (getPrime i == getPrime j)
      where
        i = abs i'
        j = abs j'

prop_compare = withQCArgs (\prop -> prop { maxSize = 256 } ) prop_compare'
  where
    prop_compare' :: (Int, Int) -> Bool
    prop_compare' (i', j') = i `compare` j == (getPrime i `compare` getPrime j)
      where
        i = abs i'
        j = abs j'

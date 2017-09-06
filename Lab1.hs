module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q1
func11_1, func11_2::Int -> Int
func11_1 n = sum[(n^2)|n<-[1..n]]
func11_2 n = (n*(n+1)*((2*n)+1)) `div` 6

test1, test1_P::Int -> Bool
test1 n = func11_1 n == func11_2 n -- fails due to negative valuations of n
test1_P n = (n >= 0) --> (func11_1 n == func11_2 n) -- introduce pre-condition to ensure validity of proof only over natural numbers

func12_1, func12_2::Int -> Int
func12_1 n = sum[(n^3)|n<-[1..n]] 
func12_2 n = ((n*(n+1)) `div` 2)^2

test2, test2_P::Int -> Bool
test2 n = func12_1 n == func12_2 n -- fails due to negative valuations of n
test2_P n = (n >= 0) --> (func12_1 n == func12_2 n) -- introduce pre-condition to ensure validity of proof only over natural numbers

-- Q2
func21_1, func21_2::Int -> Int
func21_1 n = 2^(length [1..n])
func21_2 n = length(subsequences [1..n])

prop::Int -> Bool
prop n = (25 >= n && n >= 0) -->  (func21_1 n) == (func21_2 n)

-- Q3
perms::[a]->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map(insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

prop2::Int -> Bool
prop2 n = length (perms [1..n]) == product [1..n]

-- Q4
reversal:: Integer -> Integer
reversal = read . reverse . show

factors:: Integer -> [Integer]
factors n = [k | k <- [1..n], n `mod` k == 0] -- returns a complete list of factors of n

prime:: Integer -> Bool
prime n = factors n == [1,n] -- property chekcing if n is prime based on the equivalence of its factor list and the fixed list [1,n]

primes, revPrimes::[Integer]
primes = filter prime [1..10000] --creates the list of primes via filtering on the prime property
revPrimes = filter (prime . reversal) primes  --filter list of primes on the property that the reversal of n is also prime

revProp:: Integer -> Bool
revProp n = ((n > 0) && (n `mod` 10 /= 0)) --> ((reversal $ reversal n) == n) -- n is specified as a natural number and n cannot be divisible by 10 or the reversal would have a leading zero, thus the reversal of the reversal would be the division of n and 10

--Q5
subseqOf:: Int->[a]->[[a]]
subseqOf _ [] = [[]]
subseqOf n xs = (take n xs) : subseqOf n (tail xs)

primeSumList = head (filter (prime.sum) (subseqOf 101 primes))

--Q6


--Q7


--Q8


module Codewars.Kata.Perimeter where

-- The entire Fibonacci sequence! Make sure to only use as much as you need, or else, it will never terminate :)
fibonacci :: Integral a => [a]
fibonacci = genFibonacci 1 0
  where
    genFibonacci x y = x : genFibonacci (x + y) x

-- Calculate the perimeter of some squares with side lengths of the first `n + 1` numbers in the fibonacci sequence!
perimeter :: Integer -> Integer
perimeter x = 4 * (sum $ take (succ $ fromIntegral x) $ fibonacci)

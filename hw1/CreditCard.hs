module CreditCard where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1     = []
  | otherwise = rem n 10 : toDigitsRev((div n 10))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs)
  | mod (length(xs)) 2 /= 0 = x : (2 * y) : doubleEveryOther xs
  | otherwise               = (2 * x) : y : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concat (map toDigits xs))

validate :: Integer -> Bool
validate x = rem (sumDigits (doubleEveryOther (toDigits x))) 10 == 0

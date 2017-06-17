toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
 | n <= 0 = []
 | otherwise = (mod n 10) : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n
 | n <= 0 = []
 | otherwise = toDigits (div n 10) ++ [(mod n 10)]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = myReverse (doubleOtherHelper (myReverse n))

doubleOtherHelper :: [Integer] -> [Integer]
doubleOtherHelper [] = []
doubleOtherHelper [x] = [x]
doubleOtherHelper (x : (y : ys)) = x : (2 * y) : doubleOtherHelper ys

myReverse :: [Integer] -> [Integer]
myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + (sumDigits xs) 

validate :: Integer -> Bool
validate n
 | n <= 0      = False
 | otherwise   = ((mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0)
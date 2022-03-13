import System.Environment
import Data.List
import Data.Maybe

data NestedList a = Elem a | List [NestedList a]
data EncodeList a = Single a | Multiple Int a deriving (Show)

-- My solutions to https://wiki.haskell.org/99_questions/

-- Problem 1: write myLast to find the last element in a list

myLast :: [a] -> a
myLast [] = error "There is no last element of an empty list."
myLast (_:[x]) = x
myLast (x:xs) = myLast xs

-- Problem 2: write myLast to find the second to last element in a list
myButLast :: [a] -> a
myButLast [] = error "There is no second to last element of an empty list."
myButLast x = x !! (length x - 2)

-- Problem 3: Find k-th
myKth ::  [a] -> Int -> a
myKth [] _ = error "There is no second to last element of an empty list."
myKth x k = x !! (k - 1)

-- Problem 4: Find the length of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6: Check if a palindrome
-- Not solved myself
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True -- Vacuously true
isPalindrome (x:[]) = True
isPalindrome xs = (head xs == myLast xs) && (isPalindrome $ init $ tail xs)

-- Problem 7: Flatten a nested list
-- Not solved myself
myFlatten :: NestedList a ->  [a]
myFlatten (List []) = []
myFlatten (Elem a)   = [a]
myFlatten (List (x:xs)) =   myFlatten x  ++ (myFlatten $ List (xs))

-- Problem 8: Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress xs
 | (head xs) == (head $ tail xs) = compress $ tail xs
 | otherwise = head xs : (compress $ tail xs)

-- Problem  9: Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = (takeWhile (== x) xs) : (pack $ dropWhile (== x) xs)
          where x = head xs

-- Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = ((length $ (takeWhile (== x)) xs), x) : (encode $ dropWhile (== x) xs)
            where x = head xs


-- Problem 11: Modify the result of problem 10 in such a way that if
-- an element has no duplicates it is simply copied into the result list. Only elements with duplicates are
-- transferred as (N E) lists.
encode_to_data :: [(Int, a)] -> [EncodeList a]
encode_to_data [] = []
encode_to_data (x:xs)
    | count == 1 = (Single val) :  encode_to_data xs
    | otherwise = (Multiple count val) : encode_to_data xs
        where count = fst x
              val = snd x

encodeModified :: (Eq a) => [a] -> [EncodeList a]
encodeModified = encode_to_data . encode

-- Problem 12: The inverse of encodeModified
encodeListToList :: EncodeList a -> [a]
encodeListToList (Single x) = [x]
encodeListToList (Multiple cnt val) =  [val | i <- [1..cnt]]

decodeModified :: [EncodeList a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = encodeListToList x ++ decodeModified xs


-- Problem 14: Duplicate the elements of a list.
duplicate :: [a] -> [a]
duplicate []  = []
duplicate (x:xs) = x : x : duplicate xs

-- Problem 15: Replicate the elements of a list a given number of times.
myReplicate :: [a] -> Int -> [a]
myReplicate xs cnt = [x | x <- xs, i <- [1..cnt]]

-- Problem 16: Drop every n-th entry
dropEveryHelper :: [a] -> Int -> Int -> [a]
dropEveryHelper [] _ _ = []
dropEveryHelper xs n cnt
    | (cnt `mod` n == 0) && ( cnt > 0) = dropEveryHelper  (tail xs) n (cnt + 1)
    |  otherwise = x : dropEveryHelper  (tail xs) n (cnt + 1)
        where x = head xs
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEveryHelper xs n 1

-- Problem 17: Split a list into two parts; the length of the first part is given.
splitHelper :: [a] -> Int -> Int -> Int -> [a]
splitHelper [] _ _ _ = []
splitHelper xs start end cnt
    | (cnt + start) == end = [xs !! end]
    | otherwise = (xs !! (start + cnt)) : splitHelper xs start end (cnt + 1)

split :: [a] -> Int -> ([a], [a])
split xs n = (splitHelper xs 0 (n - 1) 0, splitHelper xs n (length xs - 1) 0)

-- Problem 18: Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs start end = fst $ split ( snd $ split xs (start - 1) ) (end - start + 1)

-- Problem 19: Rotate a list N places to the left.
myRotate :: [a] -> Int -> [a]
myRotate xs n
    | n == 0 = xs
    | n > 0 = snd splitListPos ++ fst splitListPos
    | n < 0 = snd splitListNeg ++ fst splitListNeg
        where splitListPos = split xs n
              splitListNeg = split xs (length xs + n)

-- Problem 20: Remove the Kth element from a list.
removeK :: [a] -> Int -> ([a], [a])
removeK xs k = (fst splitListRight, fst splitListLeft ++ snd splitListRight)
                where splitListLeft = split xs (k - 1)
                      splitListRight = split (snd splitListLeft) 1

-- Problem 31: Test if an integer is prime.
isPrime :: Int -> Bool
isPrime x = foldl (\acc curr_num -> acc && (x `mod` curr_num /= 0)) True [2 .. (x - 1)]
isPrime x
        | x == 2 = True
        | x <= 1 = False

-- Problem 32: Find the GCD of two numbers.
myGcd :: Int -> Int -> Int
myGcd x y
    | x > y = myGcd y x
    | y `mod` x == 0 = x
    | (x < 1) || (y < 1) = error "Must both be positive integers."
myGcd x y = myGcd x (y `mod` x)

-- Problem 33: Return whether two positive integer numbers are coprime
myCoprime :: Int -> Int -> Bool
myCoprime x y
    | (x < 1) || (y < 1) = error "Must both be positive integers."
myCoprime x y = myGcd x y == 1

-- Problem 34: Calculate Euler's totient function phi(m).
phi :: Int -> Int
phi m = length $ filter (myCoprime m) [1 .. (m-1)]

-- Problem 35: Return a list of the prime factors of the input.
prime_factor_helper :: Int -> [Int] -> [Int]
prime_factor_helper x primes
    | curr_prod == x = primes
    | isPrime dividend = dividend : primes
    | otherwise = prime_factors dividend ++ primes
        where curr_prod = foldl (*) 1 primes
              dividend = x `div` curr_prod

prime_factors :: Int -> [Int]
prime_factors x =  sort $ prime_factor_helper x [prime | prime <- [2 .. (x-1)], x `mod` prime == 0, isPrime prime]

-- Problem 36: Create list of prime factors and their multiplicity
prime_multiplicity :: Int -> [(Int, Int)]
prime_multiplicity  = encode . prime_factors

-- Problem 37: Calculate Euler's totient using prime factors.
myPhiPrimes :: Int -> Int
myPhiPrimes x = foldl (*) 1  (map (\prime_mult -> (snd prime_mult - 1) * (snd prime_mult) ^ (fst prime_mult - 1)) (prime_multiplicity x))

-- Problem 39: Return a list of all the primes between a lower and upper bound.

primesR :: Int -> Int -> [Int]
primesR lower upper = [prime | prime <-[lower..upper], isPrime prime]

-- Problem 40: Calculate the two primes for an even number > 2 that add to the number. (Goldbach Conjecture)
goldbach_helper :: Int -> Int -> [Int] -> Maybe Int
goldbach_helper _ _ [] = Nothing
goldbach_helper goldbach_num candidate_prime primes
  | curr_prime + candidate_prime == goldbach_num = Just curr_prime
  | otherwise = goldbach_helper  goldbach_num candidate_prime (tail primes)
    where curr_prime = head primes

goldbach_pair :: Int -> Int -> (Int, Maybe Int)
goldbach_pair goldbach_num curr_num = (curr_num , goldbach_helper goldbach_num curr_num (primesR 2 goldbach_num) )

get_goldbachs :: Int -> [(Int, Maybe Int)]
get_goldbachs goldbach_num =   filter  (\x -> snd x /= Nothing )  (map (\x -> goldbach_pair goldbach_num x) (primesR 2 goldbach_num))

goldbach :: Int -> (Int, Int)
goldbach goldbach_num
  | goldbach_num <= 2 = error "The integer is less than 2."
  | goldbach_num `mod` 2 /= 0 = error "The integer is not even."
  | otherwise = (fst goldbach_result, fromJust $ snd goldbach_result)
    where goldbach_result = head $ get_goldbachs goldbach_num

-- Problem 41: Print all of the goldbach decompositions for the numbers in a given range.
get_goldbach_nums :: Int -> Int -> [Int]
get_goldbach_nums lower upper = filter (\x -> x `mod` 2 == 0 && x > 2) [lower .. upper]

goldbach_list :: Int -> Int -> [(Int, Int)]
goldbach_list lower upper = map  (\x -> goldbach x) (get_goldbach_nums lower upper)

main =	do print $ myLast [1 ,2 ,3]
           print $ myLast [1, 2, 3, 4]
           print $ myButLast [1, 2, 3]
           print $ myButLast ['a'.. 'z']
           print $ myKth "haskell" 5
           print $ myLength ['A'..'Z']
           print $ myReverse ['A' .. 'Z']
           print $ isPalindrome "tacocat"
           print $ myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
           print $ compress "aaabbcccxxaaddd"
           print $ pack "aabbaa"
           print $ encode "aabbaaddccc"
           print $ encodeModified "aabbaaddcccx"
           print $ decodeModified $ encodeModified "aabbaaddcccx"
           print $ duplicate $ "aaxxyya"
           print $ myReplicate "abcd" 3
           print $ dropEvery "abcdefghik" 3
           print $ split "abcdefghik" 3
           print $ slice "abcdefghik" 3 7
           print $ myRotate ['a','b','c','d','e','f','g','h'] 3
           print $ myRotate ['a','b','c','d','e','f','g','h'] (-2)
           print $ removeK "abcd" 2
           print $ isPrime 5
           print $ isPrime 4
           print $ isPrime 3
           print $ isPrime 2
           print $ myGcd 27 33
           print $ myGcd 36 63
           print $ myGcd 35 64
           print $ myCoprime 35 64
           print $ phi 10
           print $ prime_factors 315
           print $ prime_factors 12344
           print $ prime_multiplicity 315
           print $ myPhiPrimes 10
           print $ primesR 10 20
           print $ goldbach 28
           print $ goldbach_list 9 20

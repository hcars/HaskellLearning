import System.Environment   
 
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
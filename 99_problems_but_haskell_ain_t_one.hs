import System.Environment   

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


main =	do print $ myLast [1 ,2 ,3]
           print $ myLast [1, 2, 3, 4]
           print $ myButLast [1, 2, 3]
           print $ myButLast ['a'.. 'z']
           print $ myKth "haskell" 5
           print $ myLength ['A'..'Z']
           print $ myReverse ['A' .. 'Z']
           

{-
 -Title: CS3518 Haskell Assignment 2012-13
 -Due: 19 April 2013, 12 noon
 -Author: Philip Hale
 -ID: 50907446
 -}

{-
 - 1. [10 points] Write a function member which, given a string and a list of
 -    strings, returns a Boolean indicating whether the string is in the list or
 -    not. E.g.
 -
 -    Main> member "a" ["b","c","a","apple"]
 -    True
 -
 -    Main> member "a" []
 -    False
 -
 -    Main> member "" [""]
 -    True
 -
 -    Limitations: due to type definition, does not work for integers. E.g.
 -
 -    Main> member 998 [1..10000]
 -    <exception thrown due to type definition>
 -}
member :: String -> [String] -> Bool
member x []  = False
member x [y] = (x == y)
member x (y:ys)
  | y == x = True
  | otherwise = memberTail
  where memberTail = member x ys

{-
 - 2. [10 points] Write a function getelt which, given an integer n and a list,
 -    returns the nth element of the list (the first element being numbered 1).
 -    E.g.
 -
 -    Main> getelt 3 [1,2,3,4,5]
 -    3
 -}
getelt :: Int -> [x] -> x
getelt x [] = error "List is empty"
getelt x y
  | x > length y || x < 1 = error "Index out of bounds"
  | otherwise = last ( take x y )

{-
 - 3. [15 points] Write a function setelt which, given an integer n, a list (of
 -    items of any type) and some individual of this type, returns the same list
 -    but with the nth element (with the first element being numbered as 1)
 -    replaced by the one provided. E.g.
 -
 -    Main> setelt 2 [3,4,5,6] 10
 -    [3,10,5,6]
 -}
setelt :: Int -> [x] -> x -> [x]
setelt n list newElem
  | n > length list || n < 1 = error "Index out of bounds"
  | otherwise = beginning ++ newElem : end
  where (beginning,_:end) = splitAt (n-1) list

{-
 - 4. [15 points] Write a function combineWithEach which takes as arguments an
 -    integer x and a list of integers. It returns a similar list, but where 
 -    each element y has been replaced by (x+y)*(x-y), e.g.
 -
 -    Main> combineWithEach 2 [3,5,7]
 -    [-5,-21,-45]
 -}
combineWithEach :: Int -> [Int] -> [Int]
combineWithEach x [] = []
combineWithEach n (x:xs) =
  f x  : combineWithEach n xs
  where f x = (n+x)*(n-x)



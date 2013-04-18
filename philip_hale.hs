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
getelt :: Int -> [x] -> y
getelt x [] = error "List is empty"



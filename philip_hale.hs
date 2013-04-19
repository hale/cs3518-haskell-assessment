import Data.List
{-
 -Title:  CS3518 Haskell Assignment 2012-13
 -Due:    19 April 2013, 12 noon
 -Author: Philip Hale
 -ID:     50907446
 -}

{- EXERCISE 1: LIST PROCESSING -}

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
 -
 -    Main> getelt (negate 2) [1,2,3]
 -    Exception: Index out of bounds
 -
 -    Main> getelt 9 []
 -    Exception: List is empty.
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
 -
 -    Main> setelt 5 [1,2,3,4]
 -    Exception: Index out of bounds.
 -
 -    Main> setelt 9 [1..10] (negate 9)
 -    [1,2,3,4,5,6,7,8,-9,10]
 -
 -    This function first splits the list at the point where the element is to
 -    be changed.  The head of the second section of the list is discsrded -
 -    this is the element to be replaced - and a new list is created consisting
 -    of the first section, the new element, and the rest of the elements.
 -    (n-1) is used to get 1-indexing.
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
 -
 -    Main> combineWithEach 0 [0,1,2,4]
 -    [0,-1,-4,-16]
 -
 -    Main> combineWithEach (negate 2) [3,5,7]
 -    [-5,-21,-45]
 -
 -    This function recirsively iterates through the list, applying the function
 -    to each element and constructing a new processed list.
 -
 -}
combineWithEach :: Int -> [Int] -> [Int]
combineWithEach x [] = []
combineWithEach n (x:xs) =
  f x  : combineWithEach n xs
  where f x = (n+x)*(n-x)

{-
 - 5. [15 points] Write a function once which, given a list of Integers and an
 -    Integer n, returns a Boolean indicating whether n occurs exactly once in
 -    the list. E.g.
 -
 -    Main>  once [2,3,2,4] 2
 -    False
 -
 -    Main> once [1..100] 2
 -    True
 -
 -    Main> once [negate 2, 3, -2] (negate 2)
 -
 -    This function first checks that the element occurs in the list.  If it
 -    does, the element is removed from the list and the check is performed
 -    again. If it's still in the list, it's a duplicate element.  If not, the
 -    removed element must be unique in the list.
 -
 -    Limitation: It does not work on infinite lists.
 -}
once :: [Int] -> Int -> Bool
once list n
  | length list == 0 = error "List is empty"
  | n `notElem` list = False
  | otherwise = n `notElem` (delete n list)

{- EXERCISE 2: NOUGHTS AND CROSSES
 -
 - For this exercise, we will represent the state of the board of a noughts and
 - crosses game using lists of lists. We will allow the board to be of any size
 - (and there is no need for you to guarantee that all rows and columns have the
 - same length). The outer level list has elements corresponding to the rows of
 - the board, and each row is represented by the list of marks in it ("O", "X"
 - or empty).
 -}

 {-
  - 1. [10 points] Create an example of such a board, calling it "board"
  -    Declare it and its type, and any other type definitions you need. We
  -    suggest that you use an algebraic type ("Data = "), and that you use
  -    "Deriving" to make sure that the marks in a list can be printed (i.e.
  -    "shown"). (This will become relevant in part 2 of this question.)
  -
  -    This implementation uses a Type synonym for the Board, since it is
  -    simply a list of lists of Marks. More complex functionality may benefit
  -    from switching to a custom algebraic data type for the board.  The
  -    allowable marks on the board are limited to an X, O or Free which means
  -    the square is available.
  -}
data Mark = X | O | Free deriving (Show)
type Board = [[Mark]]

 {- 2. [25 points] Write a function move which, given any board (represented as
  -    above), the number of a row, the number of a column and a mark, returns a
  -    new board which is the same as the original one, except that the
  -    specified mark appears in the location identified by the row and column
  -    numbers. Test your function by applying it to the board that you defined
  -    in Part 1 of this question. Do not use the Haskell built-in function !!
  -
  -    Main> move [[X]] 1 1 O
  -    [[O]]
  -
  -    Main> let b = [[X,X,X],[O,O,O],[Free,Free,Free]]
  -    Main> move b 2 2 Free
  -    [[X,X,X],[O,Free,O],[Free,Free,Free]]
  -
  -    Main> move [[],[Free]] 1 1 O
  -    Exception: Index out of bounds.
  -
  -    Limitations: Incorrect indexing is handled by the lower level getelem
  -    and setelem functions, which throw exceptions when an invalid index is
  -    specified.
  -}
move :: Board -> Int -> Int -> Mark -> Board
move board x y mark
  | x < 1 || y < 1 = error "Index out of bounds."
  | otherwise = setelt x board (setelt y column mark)
  where column = getelt x board

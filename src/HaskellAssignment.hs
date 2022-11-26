module HaskellAssignment where

import Data.List (group)

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving (Eq)

instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst _ [] = NoMatch
findFirst needleFunc haystack
  | needleFunc haystackH = Match 0
  | otherwise = findFirstWithIndex needleFunc haystackT 1
  where
    (haystackH : haystackT) = haystack

findFirstWithIndex :: (t -> Bool) -> [t] -> Int -> Found
findFirstWithIndex _ [] _ = NoMatch
findFirstWithIndex needleFunc haystack currentIndex
  | needleFunc haystackH = Match currentIndex
  | otherwise = findFirstWithIndex needleFunc haystackT (currentIndex + 1)
  where
    (haystackH : haystackT) = haystack

------------------------------------------------
-- runLengthEncode
------------------------------------------------
data RunLength = Span Integer Char deriving (Eq)

instance Show RunLength where
  show (Span length c) = "Length: " ++ show length ++ ": " ++ show c

-- Group characters together, then map to the Span constructor
runLengthEncode :: [Char] -> [RunLength]
runLengthEncode [] = []
runLengthEncode to_encode = map (\x -> Span (toInteger (length x)) (head x)) (group to_encode)

------------------------------------------------
-- palindrome
------------------------------------------------
-- Probably not the most efficient implementation since it checks the entire string.
-- Could be optimized by only recursing to the halfway point.
-- However, this gives a decently concise, tail-recursive definition.
palindrome :: [Char] -> Bool
palindrome [] = True
palindrome candidate
  | candidateH /= last candidate = False
  | otherwise = palindrome (take (length candidateT - 1) candidateT)
  where
    (candidateH : candidateT) = candidate

------------------------------------------------
-- mergesort
------------------------------------------------
mergesort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
mergesort comparator [] = []
mergesort comparator to_sort
  | length to_sort == 1 = to_sort
  | otherwise = merge comparator (mergesort comparator left) (mergesort comparator right)
  where
    (left, right) = splitAt (length to_sort `div` 2) to_sort

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] rightList = rightList
merge _ leftList [] = leftList
merge comparator leftList rightList
  | comparator leftH rightH = leftH : merge comparator leftT rightList
  | otherwise = rightH : merge comparator leftList rightT
  where
    (leftH : leftT) = leftList
    (rightH : rightT) = rightList

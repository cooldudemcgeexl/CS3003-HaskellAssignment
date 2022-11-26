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
findFirst needleFunc (haystackH : haystackTail)
  | needleFunc haystackH = Match 0
  | otherwise = findFirstWithIndex needleFunc haystackTail 1

findFirstWithIndex :: (t -> Bool) -> [t] -> Int -> Found
findFirstWithIndex _ [] _ = NoMatch
findFirstWithIndex needleFunc (haystackH : haystackTail) currentIndex
  | needleFunc haystackH = Match currentIndex
  | otherwise = findFirstWithIndex needleFunc haystackTail (currentIndex + 1)

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
palindrome (candidateHead : candidateTail)
  | candidateHead /= last (candidateHead : candidateTail) = False
  | otherwise = palindrome (take (length candidateTail - 1) candidateTail)

------------------------------------------------
-- mergesort
------------------------------------------------
mergesort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
mergesort ordFunc [] = []
mergesort ordFunc list = list
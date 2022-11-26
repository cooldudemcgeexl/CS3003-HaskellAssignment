module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst _ [] = NoMatch
findFirst needleFunc (haystackH:haystackTail)
  | needleFunc haystackH = Match 0
  | otherwise = findFirstWithIndex needleFunc haystackTail 1

findFirstWithIndex :: (t -> Bool) -> [t] -> Int -> Found
findFirstWithIndex _ [] _ = NoMatch
findFirstWithIndex needleFunc (haystackH:haystackTail) currentIndex
  | needleFunc haystackH = Match currentIndex 
  | otherwise = findFirstWithIndex needleFunc haystackTail (currentIndex+1)
------------------------------------------------
-- runLengthEncode
------------------------------------------------
data RunLength = Span Integer Char deriving Eq
instance Show RunLength where
  show (Span length c) = "Length: " ++ show length ++ ": " ++ show c
runLengthEncode :: [Char] -> [RunLength]
runLengthEncode [] = []

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome [] = True
------------------------------------------------
-- mergesort
------------------------------------------------
mergesort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
mergesort ordFunc [] = []
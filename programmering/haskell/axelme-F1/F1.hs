{-Axel Meurling och Simon Åkesson
-}

module F1 where



import Data.Char

--To see computation time: "set +s" in ghci

-----------------------------------------------------------------------------------------------
-- Vad ska de andra funktionernas typsignaturer vara?
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

-----------------------------------------------------------------------------------------------
rovarsprak :: String -> String
karpsravor :: String -> String
vocals = "aeiouy"

vocal = flip elem vocals --what does flip elem do?

magi konsonant = konsonant : 'o' : [konsonant]

rovarsprak "" = ""
rovarsprak (letter:t) --takes a list where "letter" is the head, and "t" is the tail
 | vocal letter = letter : rovarsprak t --if vowel we add it and then call rovarsprak with the tail as argument
 | otherwise = magi letter ++ rovarsprak t --if consonant, we call "magi" to make it into röverspråket.

karpsravor "" = ""
karpsravor (letter:t)
  | vocal letter = letter : karpsravor t
  | otherwise = letter : karpsravor (drop 2 t)

-------------------------------------------------------------------------------------------------
medellangd :: String -> Double
--previous is a Bool used to check that the last element operated was a non Alphabetical character,
splitter list wordCount letters previous index listLength
  |index > listLength = letters / wordCount    --stop and return when we are at the last element
  |isAlpha (head list) && previous = splitter (tail list) (wordCount + 1) (letters + 1) False (index + 1) listLength -- when h is alphabetical and last element was a non alpha we can consider it a new word
  |isAlpha (head list) = splitter (tail list) wordCount (letters + 1) False (index + 1) listLength --just add another letter
  |otherwise = splitter (tail list) wordCount letters True (index + 1) listLength

medellangd "" = 0
medellangd list = splitter (list) 0 0 True 1 (length list)
-----------------------------------------------------------------------------------------------------
{-
@list: list that we prepend to and then reverse before returning
@remaining: the elemnts that are left that operate on
@tempList: a list where we store our even numbers
@isodd: boolean to check if we are at an even indexed element
-}

skyffla :: [a] -> [a]
shuffler list remaining tempList isOdd
  |(length remaining) + (length tempList) == 0 = reverse list
  |(length remaining == 0) = shuffler list (reverse tempList) [] True
  |isOdd = shuffler (head remaining : list) (tail remaining) tempList False
  |otherwise = shuffler list (tail remaining) (head remaining : tempList) True


skyffla list = shuffler [] list [] True

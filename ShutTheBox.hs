{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
module ShutTheBox where

import Data.Bits ((.&.))
import Data.Monoid (Sum(Sum), getSum)
import Data.Foldable (foldMap')
import Data.List (minimumBy)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (Array, (!))
import qualified Data.Array as Array

-- Interactive
shutTheBox :: (Set Int -> Int) -> IO ()
shutTheBox score = loop (Set.fromAscList [1..9])
  where
    vals = values score :: Array Int Double
    loop tiles = do
      putStrLn (showTiles tiles)
      putStr "Roll: "
      roll <- read <$> getLine
      case bestChoice vals tiles roll of
        Nothing -> putStrLn $ "Your score: " ++ show (score tiles)
        Just tiles' -> loop tiles'

showTiles :: Set Int -> String
showTiles tiles = 
  unwords (map (\i -> if Set.member i tiles then show i else " ") [1..9])

fromDigits :: [Int] -> Int
fromDigits ds = let
  n = length ds
  pows = map (10^) [n-1,n-2..0]
  in sum (zipWith (*) ds pows)

allSublists :: [a] -> [[a]]
allSublists = \case
  [] -> [[]]
  x:xs ->
    let subs = allSublists xs in
    map (x:) subs ++ subs

partitions :: Array Int [Set Int]
partitions = 
  Array.listArray (1, 12)
  (map (\x -> filter ((== x) . sum) subsets) [1..12])
  where subsets = map Set.fromAscList (allSublists [1..9])

possibilities :: Set Int -> Int -> [Set Int]
possibilities up roll =
  map (up `Set.difference`) (filter (`Set.isSubsetOf` up) (partitions ! roll)) 

bestChoice :: Ord a => Array Int a -> Set Int -> Int -> Maybe (Set Int)
bestChoice vals up roll =
  let options = possibilities up roll in
  if null options 
    then Nothing 
    else Just (minimumBy (compare `on` ((vals !) . tilesToIndex)) options)

-- bijection between {sets of positive integers} and {nonnegative integers}
-- subsets of {1..n} correspond to nonnegative integers less than 2^n
tilesToIndex :: Set Int -> Int
tilesToIndex = getSum . foldMap' (\k -> Sum (2 ^ (k - 1)))

indexToTiles :: Int -> Set Int
indexToTiles n = let
  powersOf2 = zip [1..] (takeWhile (<= n) (map (2^) [(0 :: Int)..]))
  in Set.fromList [ k | (k, p) <- powersOf2, p .&. n /= 0 ]

oneDie :: Fractional a => [(Int, a)]
oneDie = map (, 1/6) [1..6]

twoDice :: Fractional a => [(Int, a)]
twoDice = zip [2..12] (map ((/36) . fromInteger) ([1..6] ++ [5,4..1]))

weightedSum :: Num b => (a -> b) -> [(a, b)] -> b
weightedSum f = sum . map (\(x, w) -> f x * w)

values :: (Ord a, Fractional a) => (Set Int -> Int) -> Array Int a
values score = let
  arr = Array.listArray (0, 511) (map (value . indexToTiles) [0..512])
  value tiles = 
    weightedSum (\roll -> 
      let next = map ((arr !) . tilesToIndex) (possibilities tiles roll) in
      if null next then fromIntegral (score tiles) else minimum next
    )
    (if sum tiles <= 6 then oneDie else twoDice)
  in arr

digitalScore :: Set Int -> Int
digitalScore = fromDigits . Set.toAscList

sumScore :: Set Int -> Int
sumScore = sum

allOrNothing :: Set Int -> Int
allOrNothing tiles
  | Set.null tiles = 0
  | otherwise = 1

toBeat :: Int -> (Set Int -> Int) -> Set Int -> Int
toBeat x score tiles =
  case compare (score tiles) x of
    LT -> 0
    EQ -> 1
    GT -> 2
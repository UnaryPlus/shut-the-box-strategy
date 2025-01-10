{-# OPTIONS_GHC -Wall #-}
module Test (main) where

import ShutTheBox (bestChoice, values, digitalScore, sumScore, allOrNothing)
import Control.Monad (replicateM, zipWithM_)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (Array)
import Control.Monad.State (MonadState, state, evalState)
import System.Random (RandomGen, uniformR, mkStdGen)

main :: IO ()
main = do
  let gen = mkStdGen 101
  let results = evalState (testStrategies 10000 [digitalScore, sumScore, allOrNothing]) gen
  putStrLn "Strategy, mean digital score, mean sum score, mean all-or-nothing score"
  zipWithM_ (\name means -> putStrLn (commaJoin (name : map show means)))
   ["Digital", "Sum", "All-or-nothing"] results

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

commaJoin :: [String] -> String
commaJoin = foldr (\x str -> if null str then x else x ++ ", " ++ str) ""

rollDie :: (RandomGen g, MonadState g m) => m Int
rollDie = state (uniformR (1, 6))

rollDice :: (RandomGen g, MonadState g m) => Int -> m Int
rollDice n = sum <$> replicateM n rollDie

testStrategies :: (RandomGen g, MonadState g m) => Int -> [Set Int -> Int] -> m [[Double]]
testStrategies n scores = 
  mapM (scoreStrategy n scores . values) scores

scoreStrategy :: (RandomGen g, MonadState g m) => Int -> [Set Int -> Int] -> Array Int Double -> m [Double]
scoreStrategy n scores vals = do
  results <- replicateM n (runStrategy vals)
  return (map (\score -> mean (map (fromIntegral . score) results)) scores)

runStrategy :: (RandomGen g, MonadState g m) => Array Int Double -> m (Set Int)
runStrategy vals = loop (Set.fromAscList [1..9]) 
  where
    loop tiles = do
      roll <- rollDice (if sum tiles <= 6 then 1 else 2)
      case bestChoice vals tiles roll of
        Nothing -> return tiles
        Just tiles' -> loop tiles'

      
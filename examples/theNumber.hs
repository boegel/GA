{--
 - Example for GA package
 - see http://hackage.haskell.org/package/GA
 -
 - Evolve a single integer number to match the following features as closely as possible
 -   * 8 integer divisors
 -   * sum of divisors is 96
--}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad.Identity (Identity(..))
import Data.List (foldl')
import System.Random (mkStdGen, random)

import GA (Entity(..), GAConfig(..), evolve)

--
-- HELPER FUNCTIONS
--

-- find all divisors of a number
divisors :: Int -> [Int]
divisors n = concat $ map divsFor [1..(sqrt' n)]
  where
    divsFor x = if n `mod` x == 0
                     then [x, n `div` x]
                     else []

-- "integer" square root
sqrt' :: Int -> Int
sqrt' n = floor (sqrt $ fromIntegral n :: Float)

-- efficient sum
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0

--
-- GA TYPE CLASS IMPLEMENTATION
--

type Number = Int

instance Entity Number Double () () Identity where
 
  -- generate a random entity, i.e. a random integer value 
  genRandom _ seed = return $ (fst $ random $ mkStdGen seed) `mod` 10000

  -- crossover operator: sum, (abs value of) difference or (rounded) mean
  crossover _ _ seed e1 e2 = return $ Just $ case seed `mod` 3 of
                                                  0 -> e1+e2
                                                  1 -> abs (e1-e2)
                                                  2 -> (e1+e2) `div` 2
                                                  _ -> error "crossover: unknown case"

  -- mutation operator: add or subtract random value (max. 10)
  mutation _ _ seed e = return $ Just $ if seed `mod` 2 == 0
                                        then e +(1 + seed `mod` 10)
                                        else abs (e - (1 + seed `mod` 10))

  -- score: how closely does the given number match the criteria?
  -- NOTE: lower is better
  score' _ e = Just $ fromIntegral $ s + n
    where
      ds = divisors e
      s = abs $ (-) 96 $ sum' ds
      n = abs $ (-) 8 $ length ds


main :: IO() 
main = do
        let cfg = GAConfig 
                    20 -- population size
                    10 -- archive size (best entities to keep track of)
                    100 -- maximum number of generations
                    0.8 -- crossover rate (% of entities by crossover)
                    0.2 -- mutation rate (% of entities by mutation)
                    0.0 -- parameter for crossover (not used here)
                    0.2 -- parameter for mutation (% of replaced letters)
                    False -- whether or not to use checkpointing
                    False -- don't rescore archive in each generation

            g = mkStdGen 0 -- random generator

        -- Do the evolution!
        -- two last parameters (pool for generating new entities and 
        -- extra data to score an entity) are unused in this example
            (Identity es) = evolve g cfg () ()
            e = snd $ head es :: Int
        
        putStrLn $ "best entity: " ++ (show e)

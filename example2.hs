{--
 - Example for GA package
 - see http://hackage.haskell.org/package/GA
 -
 - Evolve a single integer number to match the following features as closely as possible
 -   * 8 integer divisors
 -   * sum of divisors is 96
--}

{-# LANGUAGE MultiParamTypeClasses #-}

import GA (Entity(..), GAConfig(..), ShowEntity(..), evolve)
import Data.List (foldl')
import Debug.Trace
import System (getArgs,getProgName)
import System.Random (mkStdGen, random)

--
-- HELPER FUNCTIONS
--

-- find all divisors of a number
divisors :: Int -> [Int]
divisors n = concat $ map (divsFor n) [1..(sqrt' n)]
  where
    divsFor n x = if n `mod` x == 0
                     then [x, n `div` x]
                     else []

-- "integer" square root
sqrt' :: Int -> Int
sqrt' n = floor $ sqrt $ fromIntegral n

-- efficient sum
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0

--
-- GA TYPE CLASS IMPLEMENTATION
--

instance Entity Int () () where
 
  -- generate a random entity, i.e. a random integer value 
  genRandom _ seed = (fst $ random $ mkStdGen seed) `mod` 10000

  -- crossover operator: sum, (abs value of) difference or (rounded) mean
  crossover _ _ seed e1 e2 = Just $ case seed `mod` 3 of
                                         0 -> e1+e2
                                         1 -> abs (e1-e2)
                                         2 -> (e1+e2) `div` 2

  -- mutation operator: add or subtract random value (max. 10)
  mutation _ _ seed e = Just $ if seed `mod` 2 == 0
                                  then e +(1 + seed `mod` 10)
                                  else abs (e - (1 + seed `mod` 10))

  -- score: how closely does the given number match the criteria?
  -- NOTE: lower is better
  score e _ = fromIntegral $ s + n
    where
      ds = divisors e
      s = abs $ (-) 96 $ sum' ds
      n = abs $ (-) 8 $ length ds

instance ShowEntity Int where 
  showEntity = show
 
main = do
        args <- getArgs
        progName <- getProgName
        if length args /= 8 
           then error $ "Usage: <pop. size> <archive size> <max. # generations> " ++
                               "<crossover rate> <mutation rate> " ++
                               "<crossover parameter> <mutation parameter> " ++
                               "<enable checkpointing (bool)>"
           else return ()
        let popSize       = read $ args !! 0
            archiveSize   = read $ args !! 1
            maxGens       = read $ args !! 2
            crossoverRate = read $ args !! 3
            mutationRate  = read $ args !! 4
            crossoverPar  = read $ args !! 5
            mutationPar   = read $ args !! 6
            checkpointing = read $ args !! 7
        let cfg = GAConfig 
                    popSize -- population size
                    archiveSize -- archive size (best entities to keep track of)
                    maxGens -- maximum number of generations
                    crossoverRate -- crossover rate (% of new entities generated with crossover)
                    mutationRate -- mutation rate (% of new entities generated with mutation)
                    crossoverPar -- parameter for crossover operator (not used here)
                    mutationPar -- parameter for mutation operator (ratio of replaced letters)
                    checkpointing -- whether or not to use checkpointing

            g = mkStdGen 0 -- random generator

        -- Do the evolution!
        -- two last parameters (pool for generating new entities and extra data to score an entity) are unused in this example
        e <- evolve g cfg () () :: IO Int
        
        putStrLn $ "best entity: " ++ (show e)

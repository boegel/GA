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
import System (getArgs)
import System.Random (mkStdGen, random)

import GA (Entity(..), GAConfig(..), ShowEntity(..), evolve)

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

instance Entity Number () () Identity where
 
  -- generate a random entity, i.e. a random integer value 
  genRandom _ seed = (fst $ random $ mkStdGen seed) `mod` 10000

  -- crossover operator: sum, (abs value of) difference or (rounded) mean
  crossover _ _ seed e1 e2 = Just $ case seed `mod` 3 of
                                         0 -> e1+e2
                                         1 -> abs (e1-e2)
                                         2 -> (e1+e2) `div` 2
                                         _ -> error "crossover: unknown case"

  -- mutation operator: add or subtract random value (max. 10)
  mutation _ _ seed e = Just $ if seed `mod` 2 == 0
                                  then e +(1 + seed `mod` 10)
                                  else abs (e - (1 + seed `mod` 10))

  -- score: how closely does the given number match the criteria?
  -- NOTE: lower is better
  score' e _ = fromIntegral $ s + n
    where
      ds = divisors e
      s = abs $ (-) 96 $ sum' ds
      n = abs $ (-) 8 $ length ds

instance ShowEntity Number where 
  showEntity = show

main :: IO() 
main = do
        args <- getArgs
        if length args /= 8 
           then error $ "Usage: <pop. size> <archive size> <max. # generations> " ++
                               "<crossover rate> <mutation rate> " ++
                               "<crossover parameter> <mutation parameter> " ++
                               "<enable checkpointing (bool)>"
           else return ()
        let ps  = read $ args !! 0
            as  = read $ args !! 1
            mg  = read $ args !! 2
            cr  = read $ args !! 3
            mr  = read $ args !! 4
            cp  = read $ args !! 5
            mp  = read $ args !! 6
            chk = read $ args !! 7
        let cfg = GAConfig 
                    ps -- population size
                    as -- archive size (best entities to keep track of)
                    mg -- maximum number of generations
                    cr -- crossover rate (% of new entities generated with crossover)
                    mr -- mutation rate (% of new entities generated with mutation)
                    cp -- parameter for crossover operator (not used here)
                    mp -- parameter for mutation operator (ratio of replaced letters)
                    chk -- whether or not to use checkpointing

            g = mkStdGen 0 -- random generator

        -- Do the evolution!
        -- two last parameters (pool for generating new entities and extra data to score an entity) are unused in this example
            (Identity e) = evolve g cfg () () :: Identity Int
        
        putStrLn $ "best entity: " ++ (show e)

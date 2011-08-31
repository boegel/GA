{--
 - Example for GA package
 - see http://hackage.haskell.org/package/GA
 -
 - Evolve the string "Hello World!"
--}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import GA (Entity(..), GAConfig(..), ShowEntity(..), evolve)
import Data.Char (chr,ord)
import Data.List (foldl')
import System (getArgs,getProgName)
import System.Random (mkStdGen, random, randoms)

-- efficient sum
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0

--
-- GA TYPE CLASS IMPLEMENTATION
--

instance Entity String String [Char] where
 
  -- generate a random entity, i.e. a random string
  -- assumption: max. 100 chars, only 'printable' ASCII (first 128)
  genRandom pool seed = take n $ map ((!!) pool) is
    where
        g = mkStdGen seed
        n = (fst $ random g) `mod` 101
        k = length pool
        is = map (flip mod k) $ randoms g

  -- crossover operator: mix (and trim to shortest entity)
  crossover _ _ seed e1 e2 = Just e
    where
      g = mkStdGen seed
      cps = zipWith (\x y -> [x,y]) e1 e2
      picks = map (flip mod 2) $ randoms g
      e = zipWith (!!) cps picks

  -- mutation operator: use next or previous letter randomly and add random characters (max. 9)
  mutation pool p seed e = Just $ (zipWith replace tweaks e) ++ addChars
    where
      g = mkStdGen seed
      k = round (1 / p) :: Int
      tweaks = randoms g :: [Int]
      replace i x = if (i `mod` k) == 0
                       then if even i
                               then if x > (minBound :: Char) then pred x else succ x
                               else if x < (maxBound :: Char) then succ x else pred x
                       else x
      is = map (flip mod $ length pool) $ randoms g
      addChars = take (seed `mod` 10) $ map ((!!) pool) is

  -- score: distance between current string and target
  -- sum of 'distances' between letters, large penalty for additional/short letters
  -- NOTE: lower is better
  score e x = fromIntegral $ d + 100*l
    where
      e' = map ord e
      x' = map ord x
      d = sum' $ map abs $ zipWith (-) e' x'
      l = abs $ (length x) - (length e)

instance ShowEntity String where 
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

            -- pool of characters to pick from
            charsPool = map chr [32..126]
        -- Do the evolution!
        -- Note: if either of the last two arguments is unused, just use () as a value
        e <- evolve g cfg charsPool "Hello World!" :: IO String
        
        putStrLn $ "best entity: " ++ (show e)

{--
 - Example for GA package
 - see http://hackage.haskell.org/package/GA
 -
 - Evolve the string "Hello World!"
--}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Char (chr,ord)
import Data.List (foldl')
import System.Random (mkStdGen, random, randoms)
import System.IO(IOMode(..), hClose, hGetContents, openFile)

import GA (Entity(..), GAConfig(..), 
           evolveVerbose, randomSearch)

-- efficient sum
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0

--
-- GA TYPE CLASS IMPLEMENTATION
--

type Sentence = String
type Target = String
type Letter = Char

instance Entity Sentence Double Target [Letter] IO where
 
  -- generate a random entity, i.e. a random string
  -- assumption: max. 100 chars, only 'printable' ASCII (first 128)
  genRandom pool seed = return $ take n $ map ((!!) pool) is
    where
        g = mkStdGen seed
        n = (fst $ random g) `mod` 101
        k = length pool
        is = map (flip mod k) $ randoms g

  -- crossover operator: mix (and trim to shortest entity)
  crossover _ _ seed e1 e2 = return $ Just e
    where
      g = mkStdGen seed
      cps = zipWith (\x y -> [x,y]) e1 e2
      picks = map (flip mod 2) $ randoms g
      e = zipWith (!!) cps picks

  -- mutation operator: use next or previous letter randomly and add random characters (max. 9)
  mutation pool p seed e = return $ Just $ (zipWith replace tweaks e) 
                                         ++ addChars
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
  score fn e = do
    h <- openFile fn ReadMode
    x <- hGetContents h
    length x `seq` hClose h
    let e' = map ord e
        x' = map ord x
        d = sum' $ map abs $ zipWith (-) e' x'
        l = abs $ (length x) - (length e)
    return $ Just $ fromIntegral $ d + 100*l

  -- whether or not a scored entity is perfect
  isPerfect (_,s) = s == 0.0


main :: IO() 
main = do
        let cfg = GAConfig 
                    100 -- population size
                    25 -- archive size (best entities to keep track of)
                    300 -- maximum number of generations
                    0.8 -- crossover rate (% of entities by crossover)
                    0.2 -- mutation rate (% of entities by mutation)
                    0.0 -- parameter for crossover (not used here)
                    0.2 -- parameter for mutation (% of replaced letters)
                    False -- whether or not to use checkpointing
                    False -- don't rescore archive in each generation

            g = mkStdGen 0 -- random generator

            -- pool of characters to pick from: printable ASCII characters
            charsPool = map chr [32..126]

            fileName = "goal.txt"

        -- write string to file, pretend that we don't know what it is
        -- goal is to let genetic algorithm evolve this string
        writeFile fileName "Hello World!"

        -- Do the evolution!
        -- Note: if either of the last two arguments is unused, just use () as a value
        es <- evolveVerbose g cfg charsPool fileName
        let e = snd $ head es :: String
        
        putStrLn $ "best entity (GA): " ++ (show e)

        -- Compare with random search with large budget
        -- 100k random entities, equivalent to 1000 generations of GA
        es' <- randomSearch g 100000 charsPool fileName
        let e' = snd $ head es' :: String
        
        putStrLn $ "best entity (random search): " ++ (show e')

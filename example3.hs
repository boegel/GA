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

import GA (Entity(..), GAConfig(..), evolveVerbose)

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
  mutation pool p seed e = return $ Just $ (zipWith replace tweaks e) ++ addChars
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
  score x e = return $ Just $ fromIntegral $ d + 100*l
    where
      e' = map ord e
      x' = map ord x
      d = sum' $ map abs $ zipWith (-) e' x'
      l = abs $ (length x) - (length e)

  -- whether or not a scored entity is perfect
  isPerfect (_,s) = s == 0.0


main :: IO() 
main = do
        let cfg = GAConfig 
                    100 -- population size
                    25 -- archive size (best entities to keep track of)
                    300 -- maximum number of generations
                    0.8 -- crossover rate (% of new entities generated with crossover)
                    0.2 -- mutation rate (% of new entities generated with mutation)
                    0.0 -- parameter for crossover operator (not used here)
                    0.2 -- parameter for mutation operator (ratio of replaced letters)
                    False -- whether or not to use checkpointing

            g = mkStdGen 0 -- random generator

            -- pool of characters to pick from
            charsPool = map chr [32..126]
        -- Do the evolution!
        -- Note: if either of the last two arguments is unused, just use () as a value
        es <- evolveVerbose g cfg charsPool "Hello World!"
        let e = snd $ head es :: String
        
        putStrLn $ "best entity: " ++ (show e)

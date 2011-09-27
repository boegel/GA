{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | GA, a Haskell library for working with genetic algoritms.
--
-- Aug. 2011 - Sept. 2011, by Kenneth Hoste
--
-- version: 1.0
--
-- Major features:
--
--  * flexible user-friendly API for working with genetic algorithms
--
--  * Entity type class to let user define entity definition, scoring, etc.
--
--  * abstraction over monad, resulting in a powerful yet simple interface
--
--  * support for scoring entire population at once
--
--  * support for checkpointing each generation, 
--    and restoring from last checkpoint
--
--  * convergence detection, as defined by user
--
--  * also available: random searching, user-defined progress output
--
--  * illustrative toy examples included
--
-- Hello world example:
--
-- > -- Example for GA package
-- > -- see http://hackage.haskell.org/package/GA
-- > --
-- > -- Evolve the string "Hello World!"
-- >
-- >{-# LANGUAGE FlexibleInstances #-}
-- >{-# LANGUAGE MultiParamTypeClasses #-}
-- >{-# LANGUAGE TypeSynonymInstances #-}
-- >
-- >import Data.Char (chr,ord)
-- >import Data.List (foldl')
-- >import System.Random (mkStdGen, random, randoms)
-- >import System.IO(IOMode(..), hClose, hGetContents, openFile)
-- >
-- >import GA (Entity(..), GAConfig(..), 
-- >           evolveVerbose, randomSearch)
-- >
-- >-- efficient sum
-- >sum' :: (Num a) => [a] -> a
-- >sum' = foldl' (+) 0
-- >
-- >--
-- >-- GA TYPE CLASS IMPLEMENTATION
-- >--
-- >
-- >type Sentence = String
-- >type Target = String
-- >type Letter = Char
-- >
-- >instance Entity Sentence Double Target [Letter] IO where
-- > 
-- >  -- generate a random entity, i.e. a random string
-- >  -- assumption: max. 100 chars, only 'printable' ASCII (first 128)
-- >  genRandom pool seed = return $ take n $ map ((!!) pool) is
-- >    where
-- >        g = mkStdGen seed
-- >        n = (fst $ random g) `mod` 101
-- >        k = length pool
-- >        is = map (flip mod k) $ randoms g
-- >
-- >  -- crossover operator: mix (and trim to shortest entity)
-- >  crossover _ _ seed e1 e2 = return $ Just e
-- >    where
-- >      g = mkStdGen seed
-- >      cps = zipWith (\x y -> [x,y]) e1 e2
-- >      picks = map (flip mod 2) $ randoms g
-- >      e = zipWith (!!) cps picks
-- >
-- >  -- mutation operator: use next or previous letter randomly and add random characters (max. 9)
-- >  mutation pool p seed e = return $ Just $ (zipWith replace tweaks e) 
-- >                                         ++ addChars
-- >    where
-- >      g = mkStdGen seed
-- >      k = round (1 / p) :: Int
-- >      tweaks = randoms g :: [Int]
-- >      replace i x = if (i `mod` k) == 0
-- >        then if even i
-- >          then if x > (minBound :: Char) then pred x else succ x
-- >          else if x < (maxBound :: Char) then succ x else pred x
-- >        else x
-- >      is = map (flip mod $ length pool) $ randoms g
-- >      addChars = take (seed `mod` 10) $ map ((!!) pool) is
-- >
-- >  -- score: distance between current string and target
-- >  -- sum of 'distances' between letters, large penalty for additional/short letters
-- >  -- NOTE: lower is better
-- >  score fn e = do
-- >    h <- openFile fn ReadMode
-- >    x <- hGetContents h
-- >    length x `seq` hClose h
-- >    let e' = map ord e
-- >        x' = map ord x
-- >        d = sum' $ map abs $ zipWith (-) e' x'
-- >        l = abs $ (length x) - (length e)
-- >    return $ Just $ fromIntegral $ d + 100*l
-- >
-- >  -- whether or not a scored entity is perfect
-- >  isPerfect (_,s) = s == 0.0
-- >
-- >
-- >main :: IO() 
-- >main = do
-- >        let cfg = GAConfig 
-- >                    100 -- population size
-- >                    25 -- archive size (best entities to keep track of)
-- >                    300 -- maximum number of generations
-- >                    0.8 -- crossover rate (% of entities by crossover)
-- >                    0.2 -- mutation rate (% of entities by mutation)
-- >                    0.0 -- parameter for crossover (not used here)
-- >                    0.2 -- parameter for mutation (% of replaced letters)
-- >                    False -- whether or not to use checkpointing
-- >                    False -- don't rescore archive in each generation
-- >
-- >            g = mkStdGen 0 -- random generator
-- >
-- >            -- pool of characters to pick from: printable ASCII characters
-- >            charsPool = map chr [32..126]
-- >
-- >            fileName = "goal.txt"
-- >
-- >        -- write string to file, pretend that we don't know what it is
-- >        -- goal is to let genetic algorithm evolve this string
-- >        writeFile fileName "Hello World!"
-- >
-- >        -- Do the evolution!
-- >        -- Note: if either of the last two arguments is unused, just use () as a value
-- >        es <- evolveVerbose g cfg charsPool fileName
-- >        let e = snd $ head es :: String
-- >        
-- >        putStrLn $ "best entity (GA): " ++ (show e)
-- >
-- >        -- Compare with random search with large budget
-- >        -- 100k random entities, equivalent to 1000 generations of GA
-- >        es' <- randomSearch g 100000 charsPool fileName
-- >        let e' = snd $ head es' :: String
-- >       
-- >        putStrLn $ "best entity (random search): " ++ (show e')
--

module GA (Entity(..),
           ScoredEntity, 
           Archive, 
           GAConfig(..), 
           evolve, 
           evolveVerbose,
           randomSearch) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (sortBy, nub, nubBy)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Ord (comparing)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Random (StdGen, mkStdGen, random, randoms)

-- |Currify a list of elements into tuples.
currify :: [a] -- ^ list
           -> [(a,a)] -- ^ list of tuples
currify (x:y:xs) = (x,y):currify xs
currify [] = []
currify [_] = error "(currify) ERROR: only one element left?!?"

-- |Take and drop elements of a list in a single pass.
takeAndDrop :: Int -- ^ number of elements to take/drop
            -> [a] -- ^ list 
            -> ([a],[a]) -- ^ result: taken list element and rest of list
takeAndDrop n xs
    | n > 0     = let (hs,ts) = takeAndDrop (n-1) (tail xs) 
                   in (head xs:hs, ts)
    | otherwise = ([],xs)

-- |A scored entity.
type ScoredEntity e s = (Maybe s, e)

-- |Archive of scored entities.
type Archive e s = [ScoredEntity e s]

-- |Scored generation (population and archive).
type Generation e s = ([e], Archive e s)

-- |Universe of entities.
type Universe e = [e]

-- |Configuration for genetic algorithm.
data GAConfig = GAConfig {
    -- |population size
    getPopSize :: Int, 
    -- |size of archive (best entities so far)
    getArchiveSize :: Int, 
    -- |maximum number of generations to evolve
    getMaxGenerations :: Int, 
    -- |fraction of entities generated by crossover (tip: >= 0.80)
    getCrossoverRate :: Float, 
    -- |fraction of entities generated by mutation (tip: <= 0.20)
    getMutationRate :: Float, 
    -- |parameter for crossover (semantics depend on crossover operator)
    getCrossoverParam :: Float, 
    -- |parameter for mutation (semantics depend on mutation operator)
    getMutationParam :: Float, 
    -- |enable/disable built-in checkpointing mechanism
    getWithCheckpointing :: Bool,
    -- |rescore archive in each generation?
    getRescoreArchive :: Bool
                }

-- |Type class for entities that represent a candidate solution.
--
-- Five parameters:
--
-- * data structure representing an entity (e)
--
-- * score type (s), e.g. Double
--
-- * data used to score an entity, e.g. a list of numbers (d)
--
-- * some kind of pool used to generate random entities, 
--   e.g. a Hoogle database (p)
--
-- * monad to operate in (m)
--
-- Minimal implementation should include 'genRandom', 'crossover', 'mutation', 
-- and either 'score'', 'score' or 'scorePop'.
--
-- The 'isPerfect', 'showGeneration' and 'hasConverged' functions are optional.
--
class (Eq e, Ord e, Read e, Show e, 
       Ord s, Read s, Show s, 
       Monad m)
   => Entity e s d p m 
    | e -> s, e -> d, e -> p, e -> m where

  -- |Generate a random entity. [required]
  genRandom :: p -- ^ pool for generating random entities
            -> Int -- ^ random seed
            -> m e -- ^ random entity

  -- |Crossover operator: combine two entities into a new entity. [required]
  crossover :: p -- ^ entity pool
            -> Float -- ^ crossover parameter
            -> Int -- ^ random seed
            -> e -- ^ first entity
            -> e -- ^ second entity
            -> m (Maybe e) -- ^ entity resulting from crossover

  -- |Mutation operator: mutate an entity into a new entity. [required]
  mutation :: p -- ^ entity pool
           -> Float -- ^ mutation parameter
           -> Int -- ^ random seed
           -> e -- ^ entity to mutate
           -> m (Maybe e) -- ^ mutated entity

  -- |Score an entity (lower is better), pure version. [optional]
  --
  -- Overridden if score or scorePop are implemented.
  score' :: d -- ^ dataset for scoring entities
         -> e -- ^ entity to score
         -> (Maybe s) -- ^ entity score
  score' _ _ = error $ "(GA) score' is not defined, "
                    ++ "nor is score or scorePop!"

  -- |Score an entity (lower is better), monadic version. [optional]
  --
  -- Default implementation hoists score' into monad, 
  -- overriden if scorePop is implemented.
  score :: d -- ^ dataset for scoring entities
        -> e -- ^ entity to score
        -> m (Maybe s) -- ^ entity score
  score d e = do 
                 return $ score' d e

  -- |Score an entire population of entites. [optional]
  --
  -- Default implementation returns Nothing, 
  -- and triggers indivual of entities.
  scorePop :: d -- ^ dataset to score entities
           -> [e] -- ^ universe of known entities
           -> [e] -- ^ population of entities to score
           -> m (Maybe [Maybe s]) -- ^ scores for population entities
  scorePop _ _ _ = return Nothing

  -- |Determines whether a score indicates a perfect entity. [optional]
  --
  -- Default implementation returns always False.
  isPerfect :: (e,s) -- ^ scored entity
               -> Bool -- ^ whether or not scored entity is perfect
  isPerfect _ = False

  -- |Show progress made in this generation.
  --
  -- Default implementation shows best entity.
  showGeneration :: Int -- ^ generation index
               -> Generation e s -- ^ generation (population and archive)
               -> String -- ^ string describing this generation
  showGeneration gi (_,archive) = "best entity (gen. " 
                                ++ show gi ++ "): " ++ (show e) 
                                ++ " [fitness: " ++ show fitness ++ "]"
    where
      (Just fitness, e) = head archive

  -- |Determine whether evolution should continue or not, 
  --  based on lists of archive fitnesses of previous generations.
  --
  --  Note: most recent archives are at the head of the list.
  --
  --  Default implementation always returns False.
  hasConverged :: [Archive e s] -- ^ archives so far
               -> Bool -- ^ whether or not convergence was detected
  hasConverged _ = False

-- |Initialize: generate initial population.
initPop :: (Entity e s d p m) => p -- ^ pool for generating random entities
                            -> Int -- ^ population size
                            -> Int -- ^ random seed
                            -> m [e] -- ^ initialized population
initPop pool n seed = do
                         let g = mkStdGen seed
                             seeds = take n $ randoms g
                         entities <- mapM (genRandom pool) seeds
                         return entities

-- |Binary tournament selection operator.
tournamentSelection :: (Ord s) => [ScoredEntity e s] -- ^ set of entities
                               -> Int -- ^ random seed
                               -> e -- ^ selected entity
tournamentSelection xs seed = if s1 < s2 then x1 else x2
  where
    len = length xs
    g = mkStdGen seed
    is = take 2 $ map (flip mod len) $ randoms g
    [(s1,x1),(s2,x2)] = map ((!!) xs) is

-- |Apply crossover to obtain new entites.
performCrossover :: (Entity e s d p m) => Float -- ^ crossover parameter
                                     -> Int -- ^ number of entities
                                     -> Int -- ^ random seed
                                     -> p -- ^ pool for combining entities
                                     -> [ScoredEntity e s] -- ^ entities
                                     -> m [e] -- combined entities
performCrossover p n seed pool es = do 
    let g = mkStdGen seed
        (selSeeds,seeds) = takeAndDrop (2*2*n) $ randoms g
        (crossSeeds,_) = takeAndDrop (2*n) seeds
        tuples = currify $ map (tournamentSelection es) selSeeds
    resEntities <- zipWithM ($) 
                     (map (uncurry . (crossover pool p)) crossSeeds) 
                     tuples
    return $ take n $ catMaybes $ resEntities

-- |Apply mutation to obtain new entites.
performMutation :: (Entity e s d p m) => Float -- ^ mutation parameter
                                    -> Int -- ^ number of entities
                                    -> Int -- ^ random seed
                                    -> p -- ^ pool for mutating entities
                                    -> [ScoredEntity e s] -- ^ entities
                                    -> m [e] -- mutated entities
performMutation p n seed pool es = do 
    let g = mkStdGen seed
        (selSeeds,seeds) = takeAndDrop (2*n) $ randoms g
        (mutSeeds,_) = takeAndDrop (2*n) seeds
    resEntities <- zipWithM ($) 
                     (map (mutation pool p) mutSeeds) 
                     (map (tournamentSelection es) selSeeds)
    return $ take n $ catMaybes $ resEntities

-- |Score a list of entities.
scoreAll :: (Entity e s d p m) => d -- ^ dataset for scoring entities
                               -> [e] -- ^ universe of known entities
                               -> [e] -- ^ set of entities to score
                               -> m [Maybe s]
scoreAll dataset univEnts ents = do
  scores <- scorePop dataset univEnts ents
  case scores of
    (Just ss) -> return ss
    -- score one by one if scorePop failed
    Nothing   -> mapM (score dataset) ents
 
-- |Function to perform a single evolution step:
--
-- * score all entities in the population
--
-- * combine with best entities so far (archive)
--
-- * sort by fitness
--
-- * create new population using crossover/mutation
--
-- * retain best scoring entities in the archive
evolutionStep :: (Entity e s d p m) => p -- ^ pool for crossover/mutation
                                  -> d -- ^ dataset for scoring entities
                                  -> (Int,Int,Int) -- ^ # of c/m/a entities
                                  -> (Float,Float) -- ^ c/m parameters
                                  -> Bool -- ^ rescore archive in each step?
                                  -> Universe e -- ^ known entities
                                  -> Generation e s -- ^ current generation
                                  -> Int -- ^ seed for next generation
                                  -> m (Universe e, Generation e s) 
                                     -- ^ renewed universe, next generation
evolutionStep pool
              dataset
              (cn,mn,an)
              (crossPar,mutPar)
              rescoreArchive
              universe
              (pop,archive)
              seed = do 
    -- score population
    -- try to score in a single go first
    scores <- scoreAll dataset universe pop
    archive' <- if rescoreArchive
      then return archive
      else do
        let as = map snd archive
        scores' <- scoreAll dataset universe as
        return $ zip scores' as
    let scoredPop = zip scores pop
        -- combine with archive for selection
        combo = scoredPop ++ archive'
        -- split seeds for crossover/mutation selection/seeds
        g = mkStdGen seed
        [crossSeed,mutSeed] = take 2 $ randoms g
    -- apply crossover and mutation
    crossEnts <- performCrossover crossPar cn crossSeed pool combo
    mutEnts <- performMutation mutPar mn mutSeed pool combo
    let -- new population: crossovered + mutated entities
        newPop = crossEnts ++ mutEnts
        -- new archive: best entities so far
        newArchive = take an 
                   $ nubBy (\x y -> comparing snd x y == EQ) 
                   $ sortBy (comparing fst) combo
        newUniverse = nub $ universe ++ pop
    return (newUniverse, (newPop,newArchive))

-- |Evolution: evaluate generation and continue.
evolution :: (Entity e s d p m) => GAConfig -- ^ configuration for GA
                                -> Universe e -- ^ known entities 
                                -> [Archive e s] -- ^ previous archives
                                -> Generation e s -- ^ current generation
                                -> (   Universe e
                                    -> Generation e s 
                                    -> Int 
                                    -> m (Universe e, Generation e s)
                                   ) -- ^ function that evolves a generation
                                -> [(Int,Int)] -- ^ gen indicies and seeds
                                -> m (Generation e s) -- ^evolved generation
evolution cfg universe pastArchives gen step ((_,seed):gss) = do
    (universe',nextGen) <- step universe gen seed 
    let (Just fitness, e) = (head $ snd nextGen)
        newArchive = snd nextGen
    if hasConverged pastArchives || isPerfect (e,fitness)
      then return nextGen
      else evolution cfg universe' (newArchive:pastArchives) nextGen step gss
-- no more gen. indices/seeds => quit
evolution _ _ _ gen _ [] = return gen

-- |Generate file name for checkpoint.
chkptFileName :: GAConfig -- ^ configuration for generation algorithm
              -> (Int,Int) -- ^ generation index and random seed
              -> FilePath -- ^ path of checkpoint file
chkptFileName cfg (gi,seed) = "checkpoints/GA-" 
                           ++ cfgTxt ++ "-gen" 
                           ++ (show gi) ++ "-seed-" 
                           ++ (show seed) ++ ".chk"
  where
    cfgTxt = (show $ getPopSize cfg) ++ "-" ++ 
             (show $ getArchiveSize cfg) ++ "-" ++
             (show $ getCrossoverRate cfg) ++ "-" ++
             (show $ getMutationRate cfg) ++ "-" ++
             (show $ getCrossoverParam cfg) ++ "-" ++
             (show $ getMutationParam cfg)

-- |Checkpoint a single generation.
checkpointGen :: (Entity e s d p m) => GAConfig -- ^ configuraton for GA
                                    -> Int -- ^ generation index
                                    -> Int -- ^ random seed for generation
                                    -> Generation e s -- ^ current generation
                                    -> IO() -- ^ writes to file
checkpointGen cfg index seed (pop,archive) = do
    let txt = show $ (pop,archive)
        fn = chkptFileName cfg (index,seed)
    putStrLn $ "writing checkpoint for gen " 
            ++ (show index) ++ " to " ++ fn
    createDirectoryIfMissing True "checkpoints"
    writeFile fn txt

-- |Evolution: evaluate generation, (maybe) checkpoint, continue.
evolutionVerbose :: (Entity e s d p m, 
                   MonadIO m) => GAConfig -- ^ configuration for GA
                              -> Universe e -- ^ universe of known entities
                              -> [Archive e s] -- ^ previous archives
                              -> Generation e s -- ^ current generation
                              -> (   Universe e 
                                  -> Generation e s 
                                  -> Int 
                                  -> m (Universe e, Generation e s)
                                 ) -- ^ function that evolves a generation
                              -> [(Int,Int)] -- ^ gen indicies and seeds
                              -> m (Generation e s) -- ^ evolved generation
evolutionVerbose cfg universe pastArchives gen step ((gi,seed):gss) = do
    (universe',newPa@(_,archive')) <- step universe gen seed
    let (Just fitness, e) = head archive'
    -- checkpoint generation if desired
    liftIO $ if (getWithCheckpointing cfg)
      then checkpointGen cfg gi seed newPa
      else return () -- skip checkpoint
    liftIO $ putStrLn $ showGeneration gi newPa
    -- check for perfect entity
    if hasConverged pastArchives || isPerfect (e,fitness)
       then do 
               liftIO $ putStrLn $ if isPerfect (e,fitness)
                                     then    "perfect entity found, "
                                          ++ "finished after " ++ show gi 
                                          ++ " generations!"
                                     else    "no progress for 3 generations, "
                                          ++ "stopping after " ++ show gi
                                          ++ " generations!"
               return newPa
       else evolutionVerbose cfg universe' (archive':pastArchives) newPa step gss

-- no more gen. indices/seeds => quit
evolutionVerbose _ _ _ gen _ [] = do 
    liftIO $ putStrLn $ "done evolving!"
    return gen

-- |Initialize.
initGA :: (Entity e s d p m) => StdGen  -- ^ random generator
                           -> GAConfig -- ^ configuration for GA
                           -> p -- ^ pool for generating random entities
                           -> m ([e],Int,Int,Int,
                                 Float,Float,[(Int,Int)]
                                ) -- ^ initialization result
initGA g cfg pool = do
    -- generate list of random integers
    let (seed:rs) = randoms g :: [Int]
        ps = getPopSize cfg
    -- initial population
    pop <- initPop pool ps seed
    let -- number of entities generated by crossover/mutation
        cCnt = round $ (getCrossoverRate cfg) * (fromIntegral ps)
        mCnt = round $ (getMutationRate cfg) * (fromIntegral ps)
        -- archive size
        aSize = getArchiveSize cfg
        -- crossover/mutation parameters
        crossPar = getCrossoverParam cfg
        mutPar = getMutationParam cfg
        --  seeds for evolution
        seeds = take (getMaxGenerations cfg) rs
        -- seeds per generation
        genSeeds = zip [0..] seeds
    return (pop, cCnt, mCnt, aSize, crossPar, mutPar, genSeeds)

-- |Do the evolution!
evolve :: (Entity e s d p m) => StdGen -- ^ random generator
                             -> GAConfig -- ^ configuration for GA
                             -> p -- ^ random entities pool
                             -> d -- ^ dataset required to score entities
                             -> m (Archive e s) -- ^ best entities
evolve g cfg pool dataset = do
    -- initialize
    (pop, cCnt, mCnt, aSize, 
     crossPar, mutPar, genSeeds) <- if not (getWithCheckpointing cfg)
       then initGA g cfg pool
       else error $ "(evolve) No checkpointing support " 
                 ++ "(requires liftIO); see evolveVerbose."
    -- do the evolution
    let rescoreArchive = getRescoreArchive cfg
    (_,resArchive) <- evolution 
                       cfg [] [] (pop,[]) 
                       (evolutionStep pool dataset 
                                      (cCnt,mCnt,aSize) 
                                      (crossPar,mutPar) 
                                      rescoreArchive   )
                       genSeeds
    -- return best entity
    return resArchive

-- |Try to restore from checkpoint.
--
-- First checkpoint for which a checkpoint file is found is restored.
restoreFromChkpt :: (Entity e s d p m) => GAConfig -- ^ configuration for GA
                                       -> [(Int,Int)] -- ^ gen indices/seeds
                                       -> IO (Maybe (Int,Generation e s)) 
                                          -- ^ restored generation (if any)
restoreFromChkpt cfg ((gi,seed):genSeeds) = do
    chkptFound <- doesFileExist fn
    if chkptFound 
      then do
        txt <- readFile fn
        return $ Just (gi, read txt)
      else restoreFromChkpt cfg genSeeds
  where
    fn = chkptFileName cfg (gi,seed)
restoreFromChkpt _ [] = return Nothing

-- |Do the evolution, verbosely.
--
-- Prints progress to stdout, and supports checkpointing. 
--
-- Note: requires support for liftIO in monad used.
evolveVerbose :: (Entity e s d p m, MonadIO m) 
                             => StdGen -- ^ random generator
                             -> GAConfig -- ^ configuration for GA
                             -> p -- ^ random entities pool
                             -> d -- ^ dataset required to score entities
                             -> m (Archive e s) -- ^ best entities
evolveVerbose g cfg pool dataset = do
    -- initialize
    (pop, cCnt, mCnt, aSize, 
     crossPar, mutPar, genSeeds) <- initGA g cfg pool
    let checkpointing = getWithCheckpointing cfg
    -- (maybe) restore from checkpoint
    restored <- liftIO $ if checkpointing
      then restoreFromChkpt cfg (reverse genSeeds) 
      else return Nothing
    let (gi,gen) = if isJust restored
           -- restored pop/archive from checkpoint
           then fromJust restored 
           -- restore failed, new population and empty archive
           else (-1, (pop, []))
        -- filter out seeds from past generations
        genSeeds' = filter ((>gi) . fst) genSeeds
        rescoreArchive = getRescoreArchive cfg
    -- do the evolution
    (_,resArchive) <- evolutionVerbose 
                        cfg [] [] gen 
                        (evolutionStep pool dataset 
                                       (cCnt,mCnt,aSize) 
                                       (crossPar,mutPar) 
                                       rescoreArchive)
                                       genSeeds'
    -- return best entity 
    return resArchive

-- |Random searching.
--
-- Useful to compare with results from genetic algorithm.
randomSearch :: (Entity e s d p m) => StdGen -- ^ random generator
                                   -> Int -- ^ number of random entities
                                   -> p -- ^ random entity pool
                                   -> d -- ^ scoring dataset
                                   -> m (Archive e s) -- ^ scored entities (sorted)
randomSearch g n pool dataset = do
    let seed = fst $ random g :: Int
    es <- initPop pool n seed
    scores <- scoreAll dataset [] es
    return $ nubBy (\x y -> comparing snd x y == EQ) 
           $ sortBy (comparing fst)
           $ zip scores es

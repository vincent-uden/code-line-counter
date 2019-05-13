import GlobMatcher        ( getMatches )

import ArgParser          ( Flag(..)
                          , parseArgs
                          , helpMsg
                          )

import Control.Monad      ( foldM
                          , filterM
                          )

import Data.List          ( sortBy
                          , groupBy
                          )

import System.Directory   ( listDirectory
                          , doesDirectoryExist
                          , canonicalizePath
                          )

import System.Environment ( getArgs )

import System.FilePath    ( (</>) 
                          , takeExtension
                          )

import Data.Function      ( on )

getLines :: FilePath -> IO Int
getLines path = readFile path >>= 
    \txt -> return $ txt `seq` length $ lines txt

ioSum :: [IO Int] -> IO Int
ioSum xs = foldM (\a x -> (a+) <$> x ) 0 xs

getAllFilePaths :: [Flag] -> FilePath -> IO [FilePath]
-- Recursive case
getAllFilePaths flags@[Recursive, (Ignore ignores)] path = do
    currentLevel <- listDirectory path
    -- Canonize all paths and filter out ignore paths + . and ..
    let canonCurrentLevel = filter (`notElem` ignores) $ 
            map (path </>) currentLevel
    -- Get directories for further search (if recursive flag is set)
    -- TODO
    dirs <- filterM doesDirectoryExist canonCurrentLevel
    files <- filterM ((fmap not) . doesDirectoryExist) canonCurrentLevel
    rest <- sequence $ map (getAllFilePaths flags) dirs
    return $ files ++ concat rest
-- Non-Recursive case
getAllFilePaths flags@[(Ignore ignores)] path = do
    currentLevel <- listDirectory path
    -- Canonize all paths and filter out ignore paths + . and ..
    let canonCurrentLevel = filter (`notElem` ignores) $ 
            map (path </>) $ currentLevel
    -- Filter to get files and skip directories
    files <- filterM ((fmap not) . doesDirectoryExist) canonCurrentLevel
    return files

canonIgnore :: Flag -> IO Flag
canonIgnore (Ignore paths) = (sequence (map canonicalizePath paths)) >>= 
    return . Ignore

fileExtensionGT a b = compare aEnding bEnding
    where
        aEnding = takeExtension a
        bEnding = takeExtension b

getLinesAndExt :: [FilePath] -> IO (String, Int)
getLinesAndExt paths = do
    let ext = takeExtension $ head paths
    sum <- ioSum $ map getLines paths
    return (ext, sum)

formatLinecount :: (String, Int) -> String
formatLinecount (ext, count) = ext ++ "\t: " ++ show count

formatTotal :: [(String, Int)] -> String
formatTotal xs = "Total\t: " ++ show sum
    where
        sum = foldl (\a x -> a + (snd x)) 0 xs

main = do
    args <- getArgs
    let (baseDir, endings, flags) = parseArgs args
    if Help `elem` flags
       then do
       putStrLn helpMsg

       else do
       canonBaseDir <- canonicalizePath baseDir
       -- Set up for flags
       let recursive = Recursive `elem` flags
       let ignoring = filter (\f -> case f of
                                      Ignore _ -> True
                                      _        -> False
                                      ) flags
       ignore <- case length ignoring of
                  0 -> return $ Ignore []
                  _ -> canonIgnore $ head ignoring
       -- Get all files in diredtory
       paths <- case recursive of
                    True -> getAllFilePaths [Recursive, ignore] canonBaseDir
                    False -> getAllFilePaths [ignore] canonBaseDir
       -- Filter out files with the wrong extensions
       let relevantFiles = sortBy fileExtensionGT $ 
            concatMap (getMatches paths . ("*" ++)) endings
       -- Sort then group files by extension
       let groupedByExt = groupBy ((==) `on` takeExtension) relevantFiles
       lineSums <- sequence $ map getLinesAndExt groupedByExt
       -- Format and print output
       let output = map formatLinecount lineSums
       mapM_ putStrLn output
       putStrLn $ formatTotal lineSums

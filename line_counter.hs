import GlobMatcher (getMatches)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.List (isInfixOf, isSuffixOf)

lineCounter :: String -> Int
lineCounter = length . lines

getLines :: FilePath -> IO(Int)
getLines path = do
    txt <- readFile path
    return $ (txt `seq` lineCounter txt)

ioSum :: [IO(Int)] -> Int -> IO(Int)
ioSum [] acc = do
    return acc
ioSum (x:xs) acc = do
    n <- x
    let newAcc = acc + n
    p <- ioSum xs newAcc
    return p

getAllFileLines :: FilePath -> String -> IO(Int)
getAllFileLines path ending = do
    all_files <- getDirectoryContents path
    let good_paths = getMatches ("*" ++ ending) all_files
    let all_lines = [getLines (path ++ "/" ++ p) | p <- good_paths]
    sm <- all_lines `seq` ioSum all_lines 0
    return sm

getLinesWrapper :: FilePath -> [String] -> IO(Int)
getLinesWrapper path endings = do
    let x = [getAllFileLines path end | end <- endings]
    sm <- x `seq` ioSum x 0
    return sm

parseArgs :: [String] -> (String, [String])
parseArgs xs = (path, endings)
    where
        path = head xs
        endings = tail xs

getCurrentDirs:: FilePath -> IO([FilePath])
getCurrentDirs path = do
    current_level <- getDirectoryContents path
    let dirs = filter (\x -> not ("." `isInfixOf` x)) current_level
    let combined_dirs = map (path `combinePaths`) dirs
    filtered_dirs <- filterNonExistDir combined_dirs
    if null filtered_dirs then return ([]) else do
        let more_dirs = [getCurrentDirs dir | dir <- filtered_dirs]
        let x = concatIOPaths more_dirs
        z <- x
        return $ filtered_dirs ++ z

f :: IO([FilePath]) -> IO([FilePath]) -> IO([FilePath])
f p acc = do
    path <- p
    a <- acc
    return $ path ++ a

emptyFileList :: IO([FilePath])
emptyFileList = return []

concatIOPaths :: [IO([FilePath])] -> IO([FilePath])
concatIOPaths paths = do
    folded <- foldr f emptyFileList paths
    return folded

filterNonExistDir :: [FilePath] -> IO([FilePath])
filterNonExistDir (p:ps) = do
    x <- doesDirectoryExist p
    if x then do
        rest <- filterNonExistDir ps
        return $ p:rest
    else filterNonExistDir ps
filterNonExistDir _ = return []

combinePaths :: FilePath -> FilePath -> FilePath
combinePaths path1 path2 = strippedPath1 ++ "/" ++ path2
    where strippedPath1 = if "/" `isSuffixOf` path1 then init path1 else path1

getRecLines :: FilePath -> [String]-> IO(Int)
getRecLines path endings = do
    allDirs <- getCurrentDirs path
    let mostDirs = path:allDirs
    let sums = [getLinesWrapper p endings | p <- mostDirs]
    finalSum <- sums `seq` ioSum sums 0
    return finalSum

countLinesFileType :: FilePath -> String -> IO(String)
countLinesFileType path ending = do
    result <- getRecLines path [ending]
    let output = ending ++ " : " ++ (show result) ++ " Lines"
    return output

ioPutStrLn :: [IO(String)] -> IO()
ioPutStrLn [] = do
    putStr ""
ioPutStrLn (x:xs) = do
    output <- x
    putStrLn $ output
    ioPutStrLn xs

ioLinesToStr :: (IO(Int), String) -> IO(String)
ioLinesoStr (x, ending) = do
    y <- x
    let op = show y
    return $ ending ++ "\t: " ++ op ++ "\tLines"

main = do
    args <- getArgs
    let splitArgs = parseArgs args
    let path = fst splitArgs
    let endings = snd splitArgs
    if (last endings) == "-r" then do
        let results = map (getRecLines path . (\x -> [x])) $ init endings
        let strings = map ioLinesToStr $ zip results $ init endings
        ioPutStrLn strings
    else do
        let results = map (getLinesWrapper path . (\x -> [x])) $ endings
        let strings = map ioLinesToStr $ zip results endings
        ioPutStrLn strings


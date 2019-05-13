module ArgParser ( Flag (..)
                 , parseArgs
                 , helpMsg
                 ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Flag = Help
          | Recursive
          | Ignore [FilePath]
      deriving (Show, Eq, Ord)

type FileExt = String

cmdFlags = [ "-r"
           , "-i"
           , "-h"
           ]

parseArgs :: [String] -> (FilePath, [FileExt], [Flag])
parseArgs args = (base_dir, endings, flags)
    where
        base_dir  = case head args of
                      "-h" -> ""
                      otherwise -> head args
        endings   = filter ((=='.') . head) (tail args)
        flags     = map (fromJust) $ filter (/= Nothing) [ getIgnores args 
                                                         , getRecursive args
                                                         , getHelp args
                                                         ]

getIgnores :: [String] -> Maybe(Flag)
getIgnores args = do
    index <- elemIndex "-i" args
    -- Drop everything before -i (and -i) and drop everything after -r
    return $ Ignore $ dropRep "-i" cmdFlags $ drop (index + 1) args

getRecursive :: [String] -> Maybe(Flag)
getRecursive args = if "-r" `elem` args 
                   then Just Recursive 
                   else Nothing

getHelp :: [String] -> Maybe(Flag)
getHelp args = if "-h" `elem` args || "--help" `elem` args
               then Just Help
               else Nothing

dropAfter :: Eq a => a -> [a] -> [a]
dropAfter _ []     = []
dropAfter x (y:ys) 
    | x == y    = []
    | otherwise = y:dropAfter x ys 

dropRep :: Eq a => a -> [a] -> [a] -> [a]
dropRep _ [] args = args
dropRep flag (f:fs) args
    | flag == f = dropRep flag fs args
    | otherwise = dropRep flag fs $ dropAfter f args

helpMsg = "\
\Usage: countlines [DIRECTORY] [FILE ENDINGS] [OPTIONS]\n\
\Counts the amount of lines grouped by file ending in a given directory.\n\
\\n\
\  -h, --help\tshow this message\n\
\  -r\trecursively search for files\n\
\  -i [DIRECTORIES]\tignore given driectories\n\
\\n\
\Examples:\n\n\
\  countlines . .py .html -r \n\
\  - Counts the lines in all .py and .html files in this directory and\n\
\    subdirectories.\n\
\\n\
\  countlines ~/repo .c -i ~/repo/lib -r  \n\
\  - Counts the lines in all .c files in this directory and subdirectories\n\
\    ignoring ~/repo/lib\n"


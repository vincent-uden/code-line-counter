module GlobMatcher (
        getMatches,
        isMatch
        ) where

getMatches :: String -> [String] -> [String]
getMatches pattern xs = filter (isMatch pattern) xs

isMatch :: String -> String -> Bool
isMatch "*" yss = True
isMatch ('*':xs) yss =
    let nextPart = getNextPart (getSubStr xs) yss
        dLength = length nextPart
        rest = drop dLength yss
    in  isMatch xs rest
isMatch ('?':xs) (_:ys) = isMatch xs ys
isMatch ('[':xs) (y:ys) = 
    let letters = getLetters xs
        rest = tail $ dropWhile (/=']') xs
    in  (y `elem` letters) && (isMatch rest ys)
isMatch [] (_:_) = False
isMatch (_:_) [] = False
isMatch [] []    = True
isMatch (x:xs) (y:ys) = x == y && isMatch xs ys

getSubStr :: String -> String
getSubStr xs = takeWhile (\x -> not (x `elem` "*[?")) xs

getNextPart :: String -> String -> String
getNextPart pattern [] = []
getNextPart pattern (y:ys)
    | ys `startsWith` pattern = [y]
    | otherwise = y:(getNextPart pattern ys)
        where startsWith xs p = take (length p) xs == p

getLetters :: String -> String
getLetters (x:'-':y:ys) = [x..y]
getLetters xs = takeWhile (/=']') xs

module Development.GCCXML.Declarations.PatternParser where

import Data.List
import Data.Maybe
import qualified Data.List.Utils as U
import Data.String.Utils (strip)

data Parser = Parser {
    beginChar :: Char,
    endChar   :: Char,
    seperator :: Char
} deriving (Eq, Show, Ord)


charQualifier = "'"
textQualifier = "\""
escape        = "\\"


hasPattern :: Parser -> String -> Bool
hasPattern parser string = isJust first && isJust end where
    first = elemIndex (beginChar parser) string
    end   = elemIndex (endChar parser) lastPart
    
    lastPart = case (U.split "::" string) of
        [] -> ""
        xs -> last xs


name :: Parser -> String -> String
name parser string | not $ hasPattern parser string = string
                   | otherwise = strip (slice string 0 begin) where
    (Just begin) = elemIndex (beginChar parser) string
    

findArgsSep :: Parser -> String -> Int -> Maybe Int
findArgsSep parser string start = recursive 0 (drop (start - 1) string) 0 where
    recursive _ [] _ = Nothing
    recursive index (x:xs) bktDepth
        | x == (beginChar parser) = recursive (index + 1) xs (bktDepth + 1)
        | x == (endChar parser)   = if (bktDepth == 0)
            then Just $ index + start
            else recursive (index + 1) xs bktDepth
        
        
        | x == (seperator parser) = if (bktDepth == 0)
            then Just $ index + start 
            else recursive (index + 1) xs bktDepth
                
        | otherwise = recursive (index + 1) xs bktDepth


args :: Parser -> String -> [String]
args parser string = result' where
    beg = elemIndex (beginChar parser) string
    end = elemIndex (endChar parser) string
    
    result' = if ( not (isJust beg) || not (isJust end) || (beg == end) )
        then error (string ++ ": doesn't valid template string")
        else (map strip result) where
            (beg', end') = (fromJust beg, fromJust end)
            argsOnly = slice string (beg' + 1) end'
            
            result = go 0 (findArgsSep parser argsOnly 0) []
            
            go prev Nothing      args  = args ++ [(drop (prev - 1) argsOnly)] 
            go prev (Just found) args  = res where
                res = go (found + 1) (findArgsSep parser argsOnly prev) (args ++ [(slice argsOnly prev found)])

slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i-1) $ take k xs


findArgs :: Parser -> String -> Maybe Int -> Maybe (Int, Int)
findArgs p s Nothing  = findArgs p s (Just 0)
findArgs p s (Just x) = elemIndex (beginChar p) s >>= result where
    result i = go (i + 1) (findArgsSep p s (i+1)) s >>= \x -> Just (i, x) 
    go _ Nothing _ = Nothing
    go _ _ []      = Nothing
    go prevFound (Just z) (x:xs) | x == (endChar p) = Just z
                                 | otherwise = go (z + 1) (findArgsSep p s prevFound) xs


split :: Parser -> String -> (String, [String])
split parser string = ( name parser string , args parser string )


splitRecursive :: Parser -> String -> [(String, [String])]
splitRecursive parser string | hasPattern parser string = error "Illegal Parse"
                             | otherwise = result [string] [] where
    result [] ans = ans
    result (x:xs) ans = result xs' $ (name, args) : ans where
        (name, args) = split parser x
        xs' = (reverse $ filter (hasPattern parser) args) ++ xs


join :: Parser -> String -> [String] -> Maybe String -> String
join parser name args Nothing    = join parser name args (Just ", ")
join parser name xs  (Just sep)  = result where
    result = name ++ [beginChar parser]
           ++ " " ++ concat (intersperse sep xs)
           ++ " " ++ [endChar parser]


normalize :: Parser -> String -> Maybe String -> String
normalize parser string sep | not (hasPattern parser string) = string
                            | otherwise = join parser name result sep where
    (name, args) = split parser string
    result = map (\arg -> normalize parser arg Nothing) args
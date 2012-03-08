module Development.GCCXML.Declarations.Templates where

import qualified PatternParser as P

templateParser = P.Parser '<' '>' ','

isInstantiation :: String -> Bool
isInstantiation = P.hasPattern templateParser

name :: String -> Bool
name = P.name templateParser

args :: String -> [String]
args = P.args templateParser

split :: String -> (String, [String])
split = P.split templateParser

splitRecursive :: String -> [(String, [String])]
splitRecursive = P.splitRecursive templateParser

join :: String -> [String]
join x y = P.join templateParser x y Nothing

normalize :: String -> String
normalize = P.normalize templateParser


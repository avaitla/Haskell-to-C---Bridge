module Development.GCCXML.Declarations.Templates where

import qualified PatternParser as P

callParser = P.Parser '(' ')' ','

isCallInvocation :: String -> Bool
isInstantiation = P.hasPattern callParser

name :: String -> Bool
name = P.name callParser

args :: String -> [String]
args = P.args callParser

split :: String -> (String, [String])
split = P.split templateParser

splitRecursive :: String -> [(String, [String])]
splitRecursive = P.splitRecursive templateParser

join :: String -> [String]
join x y = P.join templateParser x y Nothing


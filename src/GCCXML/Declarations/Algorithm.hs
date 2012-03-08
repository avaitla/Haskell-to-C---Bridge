module Development.GCCXML.Declarations.Algorithm where

declarationPath :: String -> Maybe Bool -> [String]
declarationPath string Nothing = declarationPath string (Just True)
declarationPath string bool = 
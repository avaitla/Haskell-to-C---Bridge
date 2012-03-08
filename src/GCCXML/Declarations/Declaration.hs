module Development.GCCXML.Declarations.Declaration where

data Location = Location {
    fileName :: FilePath,
    line     :: Int
} deriving (Eq, Show, Read, Ord)

data Declaration = Declaration {
    name         :: String,
    location     :: Location,
    isArtificial :: Bool,
    mangled      :: String,
    demangled    :: String,
    attributes   :: [String],
    parent       :: Maybe Declaration
} deriving (Eq, Show, Read)

getTopParent :: Declaration -> Declaration
getTopParent d = maybe d getTopParent (parent d)

class DeclType a where
    getDeclType :: a -> String

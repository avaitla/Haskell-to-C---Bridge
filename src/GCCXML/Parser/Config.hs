module Development.GCCXML.Parser.Config where
    
data ParserConf = ParserConf {
    workingDirectory :: FilePath,
    includePaths     :: [FilePath],
    defineSymbols    :: [String],
    undefineSymbols  :: [String],
    cFlags           :: String,
    compiler         :: Maybe String
} deriving (Eq, Show, Read)

validateParserConf :: ParserConf -> Bool
validateParserConf conf = True

data GCCXMLConf = GCCXMLConf {
    gccxmlPath            :: FilePath,
    startWithDeclarations :: [String],
    ignoreGCCXMLOutput    :: Bool,
    parserConf            :: ParserConf
} deriving (Eq, Show, Read)

validateGCCXMLConf :: GCCXMLConf -> Bool
validateGCCXMLConf conf = True
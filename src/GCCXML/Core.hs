module Development.GCCXML.Core (executeGCCXML, executeGCCXMLRaw) where

import System.Exit ( ExitCode(..) )
import System.Process ( rawSystem )
import System.IO ( hClose, hPutStrLn, openTempFile )
import System.Directory ( removeFile )
import Control.Exception.Base ( finally )

import Text.XML.Light.Input ( parseXMLDoc )
import Text.XML.Light.Types ( Element )
import qualified Data.Text.IO as TIO ( readFile )

data CxxRawTree = CxxRawTree Element

runGCCXMLSingle :: FilePath -> FilePath -> IO ExitCode
runGCCXMLSingle header output = rawSystem "gccxml" [header, "-fxml=" ++ output]

runGCCXML :: [ FilePath ] -> FilePath -> IO ExitCode
runGCCXML [ header ] outpath = runGCCXMLSingle header outpath
runGCCXML headers outpath = do
    (fp, hdl) <- openTempFile "." "tempHeaders"
    mapM_ (\hdr -> hPutStrLn hdl ("#include \"" ++ hdr ++ "\"") ) headers
    hClose hdl
    res <- runGCCXMLSingle fp outpath
    removeFile fp
    return res

runParser :: FilePath -> IO (Maybe CxxRawTree)
runParser fp = TIO.readFile fp >>= return . fmap CxxRawTree . parseXMLDoc

executeGCCXML :: [ FilePath ] -> IO (Maybe CxxRawTree)
executeGCCXML pths = do
    (fp, hdl) <- openTempFile "." "GCCXMLParse"
    hClose hdl
    let func = do
        status <- executeGCCXMLRaw pths fp
        case status of
            False -> return Nothing
            True  ->  runParser fp
    finally func (removeFile fp)
    
executeGCCXMLRaw :: [ FilePath ] -> FilePath -> IO Bool
executeGCCXMLRaw pths outPath = runGCCXML pths outPath >>= \ex -> case ex of
    ExitSuccess   -> return True
    ExitFailure _ -> return False
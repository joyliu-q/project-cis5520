module FileIO where
import System.IO ( hGetContents, hPutStr, hClose, openFile, IOMode(ReadMode, WriteMode) )


readFile :: FilePath -> IO (Maybe String)
readFile fp = do
    file <- openFile fp ReadMode
    contents <- hGetContents file
    return (Just contents)

writeFile :: FilePath -> String -> IO ()
writeFile fp s = do
    file <- openFile fp WriteMode
    hPutStr file s
    hClose file

    
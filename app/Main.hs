module Main where

import Control.Exception (catch)
import Control.Monad (unless)
import Display (generateMarkdownObj, generateMarkdownText)
import DocParse (javaDocP)
import Lib
import Parse qualified as P
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath

-- Command line:
-- Get file name
-- Calls the functions in JavaParse to convert file
-- and then calls the functions in Display to display the resulting MD

generateOutputPath :: FilePath -> FilePath -> FilePath
generateOutputPath path basePath =
  if path == basePath
    then "./generatedDocs/" ++ takeBaseName path ++ "Docs"
    else
      generateOutputPath (takeDirectory path) basePath ++ "/" ++ case takeExtension path of
        ".java" -> replaceExtension (takeFileName path) "md"
        _ -> takeFileName path

createDirectoriesAndWriteFile :: FilePath -> String -> IO ()
createDirectoriesAndWriteFile path contents = do
  putStrLn $ "Creating file " ++ path
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path contents

generateDocFile :: FilePath -> String -> IO ()
generateDocFile path basePath = do
  contents <- readFile path
  let parsedDoc = P.parse javaDocP contents
   in case parsedDoc of
        Left err -> error "unable to parse"
        Right pD ->
          let markdownObj = generateMarkdownObj pD
              markdownStr = generateMarkdownText markdownObj
              outputPath = generateOutputPath path basePath
           in createDirectoriesAndWriteFile outputPath markdownStr

generateAllDocFiles :: [FilePath] -> String -> IO ()
generateAllDocFiles (x : xs) basePath = do
  generateDocFile x basePath
  generateAllDocFiles xs basePath
generateAllDocFiles _ _ = return ()

-- main :: IO ()
-- main = do
--   recursiveFiles <- getFilesRecursive "./example/TestProject"
--   generateAllDocFiles recursiveFiles

main = do
  putStr "> "
  line <- getLine
  case line of
    "help" -> do
      putStr "Type a relative path to the directory to generate documentation for this program or type q to exit \n"
      main
    "q" -> return ()
    _ -> do
      dirExists <- doesDirectoryExist line
      if dirExists
        then do
          recursiveFiles <- getFilesRecursive line
          generateAllDocFiles recursiveFiles line
        else putStrLn "Could not locate specified directory."
      main
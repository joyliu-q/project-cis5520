module Main where

import Lib
import Parse qualified as P
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import DocParse (javaDocP)
import Display (generateMarkdownObj, generateMarkdownText)
import System.Directory.Recursive (getFilesRecursive)

-- Command line:
-- Get file name
-- Calls the functions in JavaParse to convert file
-- and then calls the functions in Display to display the resulting MD

generateOutputPath :: FilePath -> FilePath
generateOutputPath path =
  case takeDirectory path of
    "./example" -> "./example/" ++ takeBaseName path ++ "Docs"
    _ ->
      generateOutputPath (takeDirectory path) ++ "/" ++ case takeExtension path of
        ".java" -> replaceExtension (takeFileName path) "md"
        _ -> takeFileName path

createDirectoriesAndWriteFile :: FilePath -> String -> IO ()
createDirectoriesAndWriteFile path contents = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path contents

generateDocFile :: FilePath -> IO ()
generateDocFile path = do
  contents <- readFile path
  let parsedDoc = P.parse javaDocP contents
   in case parsedDoc of
        Left err -> error "unable to parse"
        Right pD ->
          let markdownObj = generateMarkdownObj pD
           in let markdownStr = generateMarkdownText markdownObj
               in createDirectoriesAndWriteFile (generateOutputPath path) markdownStr

generateAllDocFiles :: [FilePath] -> IO ()
generateAllDocFiles (x : xs) = do
  generateDocFile x
  generateAllDocFiles xs
generateAllDocFiles [] = return ()

main :: IO ()
main = do
  recursiveFiles <- getFilesRecursive "./example/TestProject"
  generateAllDocFiles recursiveFiles
  print recursiveFiles
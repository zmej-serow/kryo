module Main where

import Builder
import Files
import System.Directory.Tree
import System.Environment
import System.FilePath       (takeExtension)

main :: IO ()
main = do
  args <- getArgs
  let args = ["d:\\tmp\\static", "d:\\tmp\\site_output"] --remove when compiling to real .exe
  let pathInput = head args
  let pathOutput = args !! 1 --maybe some arg-parsing library later? MissingH?

  makeSubstrate pathInput pathOutput
  
  sourceTree <- buildSiteTree pathInput
  let treeWithPaths = zipPaths $ "" :/ dirTree sourceTree
  let siteDir = applyTemplates <$> filterDir onlyMd treeWithPaths
  let siteDirHtml = transformDir renameMdToHtml siteDir

  output <- writeDirectoryWith writeFileUtf8 $ pathOutput :/ siteDirHtml
  if not . successful $ dirTree output
    then error "Failed to add converted HTML files to output tree!"
    else putStrLn "Writing converted HTML files to output tree: OK!"

  where onlyMd (Dir ('.':_) _) = False
        onlyMd (File n _)      = takeExtension n == ".md"
        onlyMd _               = True


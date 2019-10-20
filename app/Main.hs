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
  let pathOutput = args !! 1 --maybe some arg-parsing library later?
  
  sourceTree <- buildSiteTree pathInput
  let treeWithPaths = zipPaths $ "" :/ dirTree sourceTree
  let siteDir = createSitePage <$> filterDir onlyMd treeWithPaths
  let siteDirHtml = transformDir renameMdToHtml siteDir

  -- let site = pathOutput :/ siteDirHtml
  -- вместо фильтрации md и конверсии, а потом переименования: сразу трансформдир

  -- writeDirectory site
  print siteDir
  print siteDirHtml
  return ()
  where onlyMd (Dir ('.':_) _) = False
        onlyMd (File n _)      = takeExtension n == ".md"
        onlyMd _               = True

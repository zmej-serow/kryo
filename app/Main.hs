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

  makeSubstrate pathInput pathOutput
  
  sourceTree <- buildSiteTree pathInput
  let treeWithPaths = zipPaths $ "" :/ dirTree sourceTree
  let siteDir = createSitePage <$> filterDir onlyMd treeWithPaths
  let siteDirHtml = transformDir renameMdToHtml siteDir

  -- в общем, порядок такой будет. buildSiteTree = buildHtmlTree. там только html будут.
  -- их можно сразу переименовывать из md, между прочим.
  -- и так же можно фильтрануть только НЕ md. это будет дерево из гифов, пнг, джипегов и прочей лабуды.
  -- после этого по дереву можно пройтись функцией, которая будет копировать файлы из in в out.
  -- и потом останется только записать в out дерево со сконверченными html.

  writeDirectory $ pathOutput :/ siteDirHtml
  -- print siteDir
  -- print siteDirHtml
  return ()
  where onlyMd (Dir ('.':_) _) = False
        onlyMd (File n _)      = takeExtension n == ".md"
        onlyMd _               = True


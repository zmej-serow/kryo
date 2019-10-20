module Files
  ( buildSiteTree
  , renameMdToHtml
  , Content
  ) where

import Parser
import CMark                 (commonmarkToHtml, optSmart)
import Data.Text             (Text, pack, unpack)
import Data.Maybe
import System.Directory.Tree
import System.FilePath       (takeExtension, dropExtension)
import Data.Time.Clock       (getCurrentTime)

type Content = Maybe (Data.Text.Text, Tags, DatePublished, DateOccured)

convert :: FilePath -> IO Content
convert filename = do
  c <- readFile filename
  return $ Just (toHtml c, getTags c, getDatePublished c, getDateOccured c)
  where toHtml = commonmarkToHtml [optSmart] . Data.Text.pack

convertMd :: FilePath -> IO Content
convertMd filename
  | takeExtension filename == ".md" = convert filename
  | otherwise                       = return Nothing
                
buildSiteTree :: FilePath -> IO (AnchoredDirTree Content)
buildSiteTree = readDirectoryWith convertMd

renameMdToHtml :: DirTree String -> DirTree String
renameMdToHtml (File n cs) = File (rename n) (convertz cs)
  where rename n
          | takeExtension n == ".md" = dropExtension n ++ ".html"
          | otherwise                = n
        convertz = Data.Text.unpack . commonmarkToHtml [optSmart] . Data.Text.pack
renameMdToHtml n = n
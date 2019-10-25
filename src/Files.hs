module Files
  ( buildSiteTree
  , renameMdToHtml
  , makeSubstrate
  , writeFileUtf8
  , Content
  ) where

import           Parser
import           CMark                      (commonmarkToHtml, optSmart)
import qualified Data.Text             as T (Text, pack, unpack)
import           System.IO
import           System.Directory.Tree
import           System.FilePath            (takeExtension, dropExtension)
import           Data.Time.Clock            (getCurrentTime)
import qualified Data.ByteString.Lazy  as B

type Content = Maybe (T.Text, Tags, DatePublished, DateOccured)

writeFileUtf8 :: FilePath -> String -> IO () --TODO: error handling!
writeFileUtf8 f s = do
  h <- openFile f WriteMode
  hSetEncoding h utf8
  hPutStr h s
  hClose h

readFileUtf8 :: FilePath -> IO String
readFileUtf8 f = do
  h <- openFile f ReadMode
  hSetEncoding h utf8
  hGetContents h
  
convert :: FilePath -> IO Content
convert filename = do
  c <- readFileUtf8 filename
  return $ Just (toHtml c, getTags c, getDatePublished c, getDateOccured c)
  where toHtml = commonmarkToHtml [optSmart] . T.pack

convertMd :: FilePath -> IO Content
convertMd filename
  | takeExtension filename == ".md" = convert filename
  | otherwise                       = return Nothing
                
buildSiteTree :: FilePath -> IO (AnchoredDirTree Content)
buildSiteTree = readDirectoryWith convertMd

renameMdToHtml :: DirTree a -> DirTree a
renameMdToHtml (File n cs) = File (rename n) cs
  where rename n
          | takeExtension n == ".md" = dropExtension n ++ ".html"
          | otherwise                = n
renameMdToHtml n = n

makeSubstrate :: FilePath -> FilePath -> IO () -- TODO: some logging must be. maybe, MissingH?
makeSubstrate from to = do
  input <- readDirectoryWithL B.readFile from
  if not . successful $ dirTree input
    then error "Reading source tree failed!"
    else putStrLn "Reading source tree: OK!"
  output <- writeDirectoryWith B.writeFile $ to :/ filterDir noMd (dirTree input)
  if not . successful $ dirTree output
    then error "Failed to write output tree!"
    else putStrLn "Writing non-HTML files to output tree: OK!"
  where noMd (Dir ('.':_) _) = True
        noMd (File n _)      = takeExtension n /= ".md"
        noMd _               = True
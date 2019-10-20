module Parser
  ( getDatePublished
  , getDateOccured
  , getTags
  , splitPath
  , Tags
  , DateOccured
  , DatePublished
  ) where

import Data.List       (isPrefixOf, isSuffixOf)
import Data.Time       (UTCTime, parseTimeOrError, defaultTimeLocale)

type Tags          = [String]
type DatePublished = UTCTime
type DateOccured   = UTCTime

getTags :: String -> Tags
getTags s =
  concatMap (split ',' . filter (/= ' ') . getBetweenColons)
  $ getLinesWith "tags" s

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

splitPath :: String -> [String] -- "azaza\\kokoko\\blabla\\filename.md" -> ["kokoko", "blabla"]
splitPath = init . tail . split '\\'

isSpecialLine :: String -> String -> Bool
isSpecialLine t s = (("[" ++ t ++ ":") `isPrefixOf` s) && ("]::" `isSuffixOf` s)

getBetweenColons :: String -> String
getBetweenColons = 
  let beforeColon = takeWhile (/= ':')
      afterColon  = dropWhile (/= ':')
  in init . beforeColon . tail . afterColon

timeFromString :: String -> UTCTime
timeFromString = parseTimeOrError True defaultTimeLocale "%d-%m-%Y"

getDate :: String -> String -> UTCTime
getDate t s
  | null date = timeFromString "01-01-1971"
  | otherwise = timeFromString
              . filter (/= ' ')
              . getBetweenColons
              . head $ date
  where date = getLinesWith t s

getDatePublished :: String -> UTCTime
getDatePublished = getDate "published"

getDateOccured :: String -> UTCTime
getDateOccured = getDate "date"

getLinesWith :: String -> String -> [String]
getLinesWith t = filter (isSpecialLine t) . lines

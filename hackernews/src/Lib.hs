{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Lib where

import Data.Aeson               (Value)
import Data.Aeson.Lens
import Control.Lens
import Network.HTTP.Simple

import Data.List                (group, sort, sortBy)
import Data.Text                (unpack)
import Data.Function            (on)

import Streamly
import Streamly.Prelude         (toList)

import Control.Monad.IO.Class   (liftIO)
import Control.Exception        (catch)

--
-- Concurrent monadic processing
--
listAsynclyWith :: (Foldable f) => f a -> (a -> AsyncT IO b) -> IO [b]
listAsynclyWith xs f =
    toList . asyncly $ forEachWith async xs f

--
-- Parse JSON data from an URL
--
getJson :: String -> IO (Value)
getJson url =
    httpJSON request >>= return . getResponseBody
    where
        request = parseRequest_ url

--
-- Build the URL for an item ID
--
itemURL :: Integer -> String
itemURL id =
    base ++ show id ++ extension
    where
        base = "https://hacker-news.firebaseio.com/v0/item/"
        extension = ".json"

--
-- Get the first N stories from TopStories
--
getTopStories :: Int -> IO [Integer]
getTopStories n =
    catch (getTopStoriesUnsafe) handler
    where
        getTopStoriesUnsafe :: IO [Integer]
        getTopStoriesUnsafe =
            getJson url >>= return . take n . stories
            where
                url = "https://hacker-news.firebaseio.com/v0/topstories.json"
                stories json = json ^.. values . _Integral

        handler :: JSONException -> IO [Integer]
        handler _ = return []

--
-- Get the username of the item's author
--
getAuthor :: String -> IO String
getAuthor url =
    getJson url >>= return . by
    where
        by json = unpack $ json ^. key "by" . _String

--
-- Get the commenters and the title of a given URL
--
getCommentersAndTitle :: String -> IO (String, [Integer])
getCommentersAndTitle url = do
    json <- getJson url
    return (title json, commenters json)
    where
        commenters json = ids (json ^.. key "kids")
            where
                ids [] = []
                ids (x:_) = x ^.. values . _Integral
        title json = unpack $ json ^. key "title" . _String

--
-- Aggregate commenters and titles with parallel requests
--
aggregateCommentersWithTitle :: (Foldable f) => f Integer -> IO [(String, [Integer])]
aggregateCommentersWithTitle stories =
    listAsynclyWith stories $ liftIO . getCommentersAndTitle . itemURL

--
-- Flatten the commenters lists retrived
--
allCommenters :: [(a, [b])] -> [b]
allCommenters = concat . map snd

--
-- List the titles retrived
--
allTitles :: [(a, [b])] -> [a]
allTitles = map fst

--
-- Count the number of occurence of uniques in a list
--
countUniq :: (Ord a) => [a] -> [(a, Int)]
countUniq =
    map (\xs@(x:_) -> (x, length xs)) . group . sort


--
-- Select the most frequent items from statistics
--
mostFrequent :: Int -> [(a, Int)] -> [(a, Int)]
mostFrequent n xs =
    take n $ sortBy (flip compare `on` snd) xs

--
-- Get names and number of comments of major commenters
--
mostFrequentCommenters :: Int -> [(a, [Integer])] -> IO [(String, Int)]
mostFrequentCommenters n xs =
    nameEachCommenters $ mostFrequentCommenters' n xs
    where
        mostFrequentCommenters' n xs =
            mostFrequent n $ countUniq $ allCommenters xs
        nameEachCommenters xs =
           listAsynclyWith xs $
                (\(a, b) -> (liftIO . getAuthor . itemURL) a >>= \a' -> return (a', b))

module Main where

import Lib

main :: IO ()
main = do
    stories    <- getTopStories 2
    aggregate  <- aggregateCommentersWithTitle stories
    print $ allTitles aggregate
    commenters <- mostFrequentCommenters 2 aggregate
    print $ commenters

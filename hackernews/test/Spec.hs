import Lib

import Test.QuickCheck
import Test.QuickCheck.Monadic

--
-- Assert that getAuthor returns known values on small ids
--
prop_knownAuthor :: Property
prop_knownAuthor = monadicIO $ do
    id     <- pick (elements [1..8])
    author <- run $ getAuthor (itemURL id)
    assert $  author `elem` ["pg", "phyllis", "onebeerdave", "perler", "frobnicate"]

--
-- Assert that getTopStories returns a list of expected length
--
prop_topStoriesLength :: Property
prop_topStoriesLength = monadicIO $ do
    n       <- pick (elements [1..3])
    stories <- run $ getTopStories n
    assert  $  (length stories) == n

--
-- Assert that countUniq is correct for non degenerated replicates
--
prop_countUniqReplicate :: Int -> Int -> Property
prop_countUniqReplicate n x = n > 0 ==>
    [(x, n)] == (countUniq $ replicate n x)

--
-- Run all tests
--
main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess =  3} prop_knownAuthor
    quickCheckWith stdArgs { maxSuccess =  3} prop_topStoriesLength
    quickCheckWith stdArgs { maxSuccess = 10} prop_countUniqReplicate
    putStrLn "Done"

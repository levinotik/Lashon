module Lashon.Text
  ( bigrams 
  ) where 

import qualified Data.Text as T
import Data.Text (Text)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances

-- | A bigram is a pair of adjacent words in text
-- 
-- Examples: 
--
-- >>> :set -XOverloadedStrings
-- >>> bigrams "more is said than done"
-- [("more","is"),("is","said"),("said","than"),("than","done")]
-- 
-- >>> bigrams "alice's adventures in wonderland"
-- [("alice's","adventures"),("adventures","in"),("in","wonderland")]
-- 
-- prop> \t -> (length (bigrams t)) == max 0 ((length $ T.words t) - 1)
bigrams :: Text -> [(Text, Text)]
bigrams t = couples (T.words t) [] 
            where 
              couples :: [Text] -> [(Text, Text)] -> [(Text, Text)]
              couples words acc = case words of 
                [x, y]   -> acc ++ [(x, y)]
                (h:m:t)  -> couples (m:t) (acc ++ [(h, m)])
                _        -> acc
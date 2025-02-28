{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Anagrams where

import           Basement.Compat.Base
import           Course.Core
import           Course.Functor
import           Course.List

{-

Functions you will need
--
\* fmap :: (a -> b) -> IO a -> IO b
\* readFile :: FilePath -> IO Str
\* lines :: Str -> [Str]
\* permutations :: [a] -> [[a]]
\* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
\* toLower :: Char -> Char

Functions that might help
-
\* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars ->
  FilePath ->
  IO (List Chars)
anagrams input filePath = do
  let perms = permutations input
  dictionary <- fmap lines (readFile filePath)
  pure $ intersectBy equalIgnoringCase dictionary perms

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars ->
  Chars ->
  Bool
equalIgnoringCase a b =
  map toLower a == map toLower b

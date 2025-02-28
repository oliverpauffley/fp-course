{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import qualified Data.Set           as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams ::
  Chars ->
  FilePath ->
  IO (List Chars)
fastAnagrams input filePath = do
  let perms = foldr (S.insert . NoCaseString) S.empty (permutations input)

  dictionary <- map NoCaseString . lines <$> readFile filePath

  let filtered = (filter (flip S.member perms) dictionary)
  pure $ map ncString filtered

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f b (x :. xs) = f x (foldr f b xs)
foldr _ b Nil       = b

newtype NoCaseString
  = NoCaseString
      Chars
  deriving (Ord)

ncString ::
  NoCaseString ->
  Chars
ncString (NoCaseString s) =
  s

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

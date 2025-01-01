{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FileIO where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad
import           Data.Foldable      (traverse_)

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

\$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath ->
  Chars ->
  IO ()
printFile filePath chars = do
  putStr $ replicate 12 '=' ++ " "
  putStrLn filePath
  putStrLn chars

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars) ->
  IO ()
printFiles Nil = pure ()
printFiles (x :. xs) = do
  uncurry printFile x
  printFiles xs

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath ->
  IO (FilePath, Chars)
getFile path = do
  contents <- readFile path
  pure (path, contents)

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath ->
  IO (List (FilePath, Chars))
getFiles xs = sequence $ getter xs
  where
    getter Nil = Nil
    getter (y :. ys) = do
      (getFile y :. Nil) ++ getter ys

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@, @lines@, and @printFiles@.
run ::
  FilePath ->
  IO ()
run path = do
  contents <- lines <$> readFile path
  printFiles =<< getFiles contents

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = do
  (args :. Nil) <- getArgs
  run args

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.

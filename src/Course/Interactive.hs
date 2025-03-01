{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Interactive where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad
import           Course.Optional
import           Course.Traversable

-- | Eliminates any value over which a functor is defined.
vooid ::
  (Functor m) =>
  m a ->
  m ()
vooid =
  (<$>) (const ())

-- | A version of @bind@ that ignores the result of the effect.
(>-) ::
  (Monad m) =>
  m a ->
  m b ->
  m b
(>-) a =
  (>>=) a . const

-- | Runs an action until a result of that action satisfies a given predicate.
untilM ::
  (Monad m) =>
  -- | The predicate to satisfy to stop running the action.
  (a -> m Bool) ->
  -- | The action to run until the predicate satisfies.
  m a ->
  m a
untilM p a =
  a >>= \r ->
    p r >>= \q ->
      if q
        then pure r
        else untilM p a

-- | Example program that uses IO to echo back characters that are entered by the user.
echo ::
  IO ()
echo =
  vooid
    ( untilM
        ( \c ->
            if c == 'q'
              then
                putStrLn "Bye!"
                  >- pure True
              else pure False
        )
        ( putStr "Enter a character: "
            >- getChar
            >>= \c ->
              putStrLn ""
                >- putStrLn (c :. Nil)
                >- pure c
        )
    )

data Op
  = Op Char Chars (IO ()) -- keyboard entry, description, program

-- |
--
-- * Ask the user to enter a string to convert to upper-case.
--
-- * Convert the string to upper-case.
--
-- * Print the upper-cased string to standard output.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @toUpper :: Char -> Char@ -- (Data.Char) converts a character to upper-case.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
convertInteractive ::
  IO ()
convertInteractive = do
  putStr "enter a string to convert to upper-case.\n"
  string <- getLine
  putStrLn ""
  putStrLn $ map toUpper string

-- |
--
-- * Ask the user to enter a file name to reverse.
--
-- * Ask the user to enter a file name to write the reversed file to.
--
-- * Read the contents of the input file.
--
-- * Reverse the contents of the input file.
--
-- * Write the reversed contents to the output file.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @readFile :: FilePath -> IO String@ -- an IO action that reads contents of a file.
--
-- /Tip:/ @writeFile :: FilePath -> String -> IO ()@ -- writes a string to a file.
--
-- /Tip:/ @reverse :: [a] -> [a]@ -- reverses a list.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
reverseInteractive ::
  IO ()
reverseInteractive = do
  putStrLn "enter a file name to reverse"
  fileName <- getLine
  putStrLn "enter a file name to write too"
  newFile <- getLine

  f <- readFile fileName

  writeFile newFile (reverse f)

-- |
--
-- * Ask the user to enter a string to url-encode.
--
-- * Convert the string with a URL encoder.
--
-- * For simplicity, encoding is defined as:
--
-- * @' ' -> \"%20\"@
--
-- * @'\t' -> \"%09\"@
--
-- * @'\"' -> \"%22\"@
--
-- * @/anything else is unchanged/@
--
-- * Print the encoded URL to standard output.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
encodeInteractive ::
  IO ()
encodeInteractive = do
  putStrLn "enter string to encode"
  inp <- getLine
  putStrLn $ flatMap enc inp
  where
    enc ' '  = "%20"
    enc '\t' = "%09"
    enc '\"' = "%22"
    enc a    = a :. Nil

interactive ::
  IO ()
interactive =
  let ops =
        ( Op 'c' "Convert a string to upper-case" convertInteractive
            :. Op 'r' "Reverse a file" reverseInteractive
            :. Op 'e' "Encode a URL" encodeInteractive
            :. Op 'q' "Quit" (pure ())
            :. Nil
        )
   in vooid
        ( untilM
            ( \c ->
                if c == 'q'
                  then
                    putStrLn "Bye!"
                      >- pure True
                  else pure False
            )
            ( putStrLn "Select: "
                >- traverse
                  ( \(Op c s _) ->
                      putStr (c :. Nil)
                        >- putStr ". "
                        >- putStrLn s
                  )
                  ops
                >- getChar
                >>= \c ->
                  putStrLn ""
                    >- let o = find (\(Op c' _ _) -> c' == c) ops
                           r = case o of
                             Empty -> (putStrLn "Not a valid selection. Try again." >-)
                             Full (Op _ _ k) -> (k >-)
                        in r (pure c)
            )
        )

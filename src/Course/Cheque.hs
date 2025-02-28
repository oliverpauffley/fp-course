{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad
import           Course.Optional
import           Data.Maybe         (catMaybes)

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh
          [ const "",
            const "un",
            const "do",
            const "tre",
            const "quattuor",
            const "quin",
            const "sex",
            const "septen",
            const "octo",
            \q -> if "n" `isPrefixOf` q then "novem" else "noven"
          ]
      postillion ::
        List Chars
      postillion =
        listh
          [ "vigintillion",
            "trigintillion",
            "quadragintillion",
            "quinquagintillion",
            "sexagintillion",
            "septuagintillion",
            "octogintillion",
            "nonagintillion",
            "centillion",
            "decicentillion",
            "viginticentillion",
            "trigintacentillion",
            "quadragintacentillion",
            "quinquagintacentillion",
            "sexagintacentillion",
            "septuagintacentillion",
            "octogintacentillion",
            "nonagintacentillion",
            "ducentillion",
            "deciducentillion",
            "vigintiducentillion",
            "trigintaducentillion",
            "quadragintaducentillion",
            "quinquagintaducentillion",
            "sexagintaducentillion",
            "septuagintaducentillion",
            "octogintaducentillion",
            "nonagintaducentillion",
            "trecentillion",
            "decitrecentillion",
            "vigintitrecentillion",
            "trigintatrecentillion",
            "quadragintatrecentillion",
            "quinquagintatrecentillion",
            "sexagintatrecentillion",
            "septuagintatrecentillion",
            "octogintatrecentillion",
            "nonagintatrecentillion",
            "quadringentillion",
            "deciquadringentillion",
            "vigintiquadringentillion",
            "trigintaquadringentillion",
            "quadragintaquadringentillion",
            "quinquagintaquadringentillion",
            "sexagintaquadringentillion",
            "septuagintaquadringentillion",
            "octogintaquadringentillion",
            "nonagintaquadringentillion",
            "quingentillion",
            "deciquingentillion",
            "vigintiquingentillion",
            "trigintaquingentillion",
            "quadragintaquingentillion",
            "quinquagintaquingentillion",
            "sexagintaquingentillion",
            "septuagintaquingentillion",
            "octogintaquingentillion",
            "nonagintaquingentillion",
            "sescentillion",
            "decisescentillion",
            "vigintisescentillion",
            "trigintasescentillion",
            "quadragintasescentillion",
            "quinquagintasescentillion",
            "sexagintasescentillion",
            "septuagintasescentillion",
            "octogintasescentillion",
            "nonagintasescentillion",
            "septingentillion",
            "deciseptingentillion",
            "vigintiseptingentillion",
            "trigintaseptingentillion",
            "quadragintaseptingentillion",
            "quinquagintaseptingentillion",
            "sexagintaseptingentillion",
            "septuagintaseptingentillion",
            "octogintaseptingentillion",
            "nonagintaseptingentillion",
            "octingentillion",
            "decioctingentillion",
            "vigintioctingentillion",
            "trigintaoctingentillion",
            "quadragintaoctingentillion",
            "quinquagintaoctingentillion",
            "sexagintaoctingentillion",
            "septuagintaoctingentillion",
            "octogintaoctingentillion",
            "nonagintaoctingentillion",
            "nongentillion",
            "decinongentillion",
            "vigintinongentillion",
            "trigintanongentillion",
            "quadragintanongentillion",
            "quinquagintanongentillion",
            "sexagintanongentillion",
            "septuagintanongentillion",
            "octogintanongentillion",
            "nonagintanongentillion"
          ]
   in listh
        [ "",
          "thousand",
          "million",
          "billion",
          "trillion",
          "quadrillion",
          "quintillion",
          "sextillion",
          "septillion",
          "octillion",
          "nonillion",
          "decillion",
          "undecillion",
          "duodecillion",
          "tredecillion",
          "quattuordecillion",
          "quindecillion",
          "sexdecillion",
          "septendecillion",
          "octodecillion",
          "novemdecillion"
        ]
        ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord, Show)

showDigit ::
  Digit ->
  Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

digitTens ::
  Digit ->
  Chars
digitTens Zero = ""
digitTens One =
  "ten"
digitTens Two =
  "twenty"
digitTens Three =
  "thirty"
digitTens Four =
  "fourty"
digitTens Five =
  "fifty"
digitTens Six =
  "sixty"
digitTens Seven =
  "seventy"
digitTens Eight =
  "eighty"
digitTens Nine =
  "ninety"

showTens ::
  Digit ->
  Chars
showTens Zero = digitTens One
showTens One =
  "eleven"
showTens Two =
  "twelve"
showTens Three =
  "thirteen"
showTens Four =
  "fourteen"
showTens Five =
  "fiftenn"
showTens Six =
  "sixteen"
showTens Seven =
  "seventeen"
showTens Eight =
  "eighteen"
showTens Nine =
  "nineteen"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3
  = D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

-- Possibly convert a character to a digit.
fromChar ::
  Char ->
  Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars ->
  Chars
dollars xs = case splitOn '.' xs of
  (xs, Nil) -> toDollars xs ++ " and " ++ "zero cents"
  ("1", xs) -> "one dollar" ++ " and " ++ toCents xs
  (Nil, xs) -> "zero dollars" ++ " and " ++ toCents xs
  (xs, "1") -> toDollars xs ++ " and " ++ "one cent"
  (ys, xs)  -> toDollars ys ++ " and " ++ toCents xs
  where
    toCents xs = toWords xs ++ "cents"
    toDollars xs = toWords xs ++ "dollars"
    toWords = digitsWithIllionToChars . reverse . withIllion . groupChars . cleaned
    cleaned = filter (\s -> isDigit s || s == '.')

splitOn :: (Eq a) => a -> List a -> (List a, List a)
splitOn c zs = (xs, drop 1 ys)
  where
    (xs, ys) = span (/= c) zs

digitsWithIllionToChars :: List (Digit3, Chars) -> Chars
digitsWithIllionToChars = flatten . intercalate " " . map digitWithIllionToChars

intercalate :: Chars -> List Chars -> List Chars
intercalate _ Nil       = Nil
intercalate c (x :. xs) = x :. c :. intercalate c xs

digitWithIllionToChars :: (Digit3, Chars) -> Chars
digitWithIllionToChars (d, "") = digitToChars d
digitWithIllionToChars (d, i)  = digitToChars d ++ " " ++ i

digitToChars :: Digit3 -> Chars
digitToChars (D3 Zero t u) = digitToChars (D2 t u)
digitToChars (D3 h t u) = showDigit h ++ " hundred and " ++ digitTens t ++ "-" ++ showDigit u
digitToChars (D2 One u) = showTens u
digitToChars (D2 t u) = digitTens t ++ " " ++ showDigit u
digitToChars (D1 u) = showDigit u

withIllion :: List Digit3 -> List (Digit3, Chars)
withIllion = flip zip illion

groupChars :: Chars -> List Digit3
groupChars = group . catOptional . map fromChar . reverse

group :: List Digit -> List Digit3
group Nil                 = Nil
group (a :. b :. c :. xs) = D3 c b a :. group xs
group (a :. b :. xs)      = D2 b a :. group xs
group (a :. xs)           = D1 a :. group xs

catOptional :: List (Optional a) -> List a
catOptional Nil            = Nil
catOptional (Empty :. xs)  = catOptional xs
catOptional (Full x :. xs) = x :. catOptional xs

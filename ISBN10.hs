module ISBN10 (
  validISBN10
  ) where

import Data.List (intersperse)

validISBN10 :: String -> Bool
validISBN10 cs = and
  [ validLength cs,
    validFirstRanges cs,
    validLastRange cs,
    validNumbers cs ]

validLength :: String -> Bool
validLength = (== 10) . length

validFirstRanges :: String -> Bool
validFirstRanges = all id . map (`elem` ['0'..'9']) . take 9

validLastRange :: String -> Bool
validLastRange []     = False
validLastRange [x]    = or [x `elem` ['0'..'9'], x == 'X']
validLastRange (_:xs) = validLastRange xs

formatString :: String -> String
formatString cs = '[' : intersperse ',' cs ++ "]"

validNumbers :: String -> Bool
validNumbers = validNumbers' . read . formatString
    where
        validNumbers' ns = (sum $ zipWith (*) ns [1..]) `mod` 11 == 0

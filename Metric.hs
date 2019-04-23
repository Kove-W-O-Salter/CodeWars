module Metric where

import System.IO.Unsafe

-- meters :: (Num a, Fractional a, Show a) => a -> IO String
meters :: Integer -> IO String
meters n | and [l >= 1, l < 4]   = show n ++ "m"
         | and [l >= 4, l < 7]   = show ((fromIntegral n) / 1000) ++ "km"
         | and [l >= 7, l < 10]  = show ((fromIntegral n) / 1000000) ++ "Mm"
         | and [l >= 10, l < 13] = show ((fromIntegral n) / 1000000000) ++ "Gm"
         | and [l >= 13, l < 16] = show ((fromIntegral n) / 1000000000000) ++ "Pm"
         | and [l >= 16, l < 19] = show ((fromIntegral n) / 1000000000000000) ++ "Em"
         | and [l >= 19, l < 22] = show ((fromIntegral n) / 1000000000000000000) ++ "Zm"
         | and [l >= 22, l < 25] = show ((fromIntegral n) / 1000000000000000000000) ++ "Ym"
         where
           l = length $ show n
           kilometre = 10^3
           megametre = 10^6
           gigametre = 10^9
           terametre = 10^12
           petametre = 10^15
           exametre = 10^18
           

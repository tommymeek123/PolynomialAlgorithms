module Main where

import Data.Char.SScript (formatSS)
import Polynomial
import RingParams
import BaseRing

params = RingParams {field=Q, order=Lex, numVars=30}

main :: IO ()
main = do
    let fstring = "-4x_3^4 x_2 x_5^2 - 7/2 x_1^2 x_3 x_2^5 x_9^3 + 6 x_2^4 x_5^3 + 9/3 x_4^9x_3^9x_2^9"
        gstring = "25x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4 + 7x_3^3 x_2^2 x_5^2 x_8^2 x_7 x_4 + x_13^9x_2^4 x_6^2"
        hstring = "x_1^5 + 2x_1^7 - 4x_1^8 + x_1 + 35/15 + 2x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4"
        f = (fromString params fstring) :: (Poly Q)
        g = (fromString params gstring) :: (Poly Q)
        h = (fromString params hstring) :: (Poly Q)
    putStrLn $ "f = " ++ (formatSS . show) f
    putStrLn $ "g = " ++ (formatSS . show) g
    putStrLn $ "h = " ++ (formatSS . show) h
    putStrLn $ "LT(f) = " ++ (formatSS . show $ leadTerm f)
    putStrLn $ "LT(g) = " ++ (formatSS . show $ leadTerm g)
    putStrLn $ "LT(h) = " ++ (formatSS . show $ leadTerm h)
    putStrLn $ "LM(f) = " ++ (formatSS . show $ leadMonomP f)
    putStrLn $ "LM(g) = " ++ (formatSS . show $ leadMonomP g)
    putStrLn $ "LM(h) = " ++ (formatSS . show $ leadMonomP h)
    putStrLn $ "g + h = " ++ (formatSS . show $ g `add` h)
    print $ totalDegree f
    print $ totalDegree g
    print $ totalDegree h

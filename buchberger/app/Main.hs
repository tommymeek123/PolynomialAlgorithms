module Main where

import Polynomial
import RingParams

params = RingParams {field=Fp, order=Grlex, numVars=9}

main :: IO ()
main = do
    let fstring = "-4x_3^4 x_2 x_5^2 - 7/2 x_1^2 x_3 x_2^5 x_9^3 + 6 x_2^4 x_5^3 + 9/3 x_4^9x_3^9x_2^9"
        gstring = "25x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4 + 7x_3^3 x_2^2 x_5^2 x_8^2 x_7 x_4 + x_2^4 x_6^7"
        hstring = "x_1^5 + 2x_1^7 - 4x_1^8 + x_1 + 35/15"
        f = fromString params fstring
        g = fromString params gstring
        h = fromString params hstring
    putStrLn $ "f = " ++ format f
    putStrLn $ "h = " ++ format h
    print g
    print $ leadCoef h
    print $ leadMonom h
    putStrLn $ leadMonomF h

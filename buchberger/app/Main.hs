module Main where

import Polynomial
import RingParams

params = RingParams {field=Q, monOrder=Grlex, numVars=9}

main :: IO ()
main = do
--    let m1 = "x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4"
--        m2 = "x_3^3 x_2^2 x_5^2 x_8^2 x_7 x_4"
--        m3 = "x_1^2 x_3 x_2^5 x_9^3"
--        m4 = "x_3^4 x_2 x_5^2"
    let fstring = "-4x_3^4 x_2 x_5^2 - 7 x_1^2 x_3 x_2^5 x_9^3 + 6 x_2^4 x_5^3 + 9 x_4^9x_3^9x_2^9"
    let gstring = "25x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4 + 7x_3^3 x_2^2 x_5^2 x_8^2 x_7 x_4 + x_2^4 x_6^7"
    let hstring = "x^5 + 2x^7 - 4x^8 + x + 35"
    let f = fromString params fstring
        g = fromString params gstring
        h = fromString params hstring
    putStrLn $ format f
    print g
    print $ leadCoef f
    print $ leadMonom f
--        fgProd = f `monMult` g
--        hiProd = h `monMult` i
--    print h
--    print i
--    print hiProd
--    print fgProd
--    putStrLn $ format f
--    putStrLn $ format g
--    putStrLn $ format h
--    putStrLn $ format fgProd
--    print $ fgProd > f
--    print $ fgProd > g
--    print $ f == g
--    print $ fgProd == fgProd
--    print $ f <= g
--    print $ h > g

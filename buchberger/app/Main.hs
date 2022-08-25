module Main where

import DenseMonom

order = Grevlex
numVars = 9

main :: IO ()
main = do
    let f = fromString "x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4" numVars order
        g = fromString "x_3^3 x_2^2 x_5^2 x_8^2 x_7 x_4" numVars order
        h = fromString "x_1^2 x_3 x_2^5 x_9^3" numVars order
        i = fromString "x_3^4 x_2 x_5^2" numVars order
        fgProd = f `monMult` g
        hiProd = h `monMult` i
    print h
    print i
    print hiProd
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


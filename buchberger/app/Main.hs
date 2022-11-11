module Main where

import Data.Char.SScript (formatSS)
import qualified DenseMonom as M
import qualified RingParams as RP

main :: IO ()
main = do
    let fstring = "x_3^4 x_2 x_5^2"
        gstring = "x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4"
        hstring = "x_2^5"
        f = M.fromString fstring :: M.Monomial RP.Lex 9
        g = M.fromString gstring :: M.Monomial RP.Lex 5
        h = M.fromString hstring :: M.Monomial RP.Lex 9
        i = mempty :: M.Monomial RP.Lex 5
    putStrLn $ "f = " ++ (formatSS . show) f
    putStrLn $ "g = " ++ (formatSS . show) g
    putStrLn $ "h = " ++ (formatSS . show) h
    putStrLn $ "fh = " ++ (formatSS . show) (f <> h)
    putStrLn $ "gi = " ++ (formatSS . show) (g <> i)

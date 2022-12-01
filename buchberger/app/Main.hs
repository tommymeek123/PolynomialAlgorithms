module Main where

import Data.Char.SScript (formatSS)
import qualified Polynomial as P
import qualified RingParams as RP
import PolyParsers (Readable(..))

main :: IO ()
main = do
--    let fmstring = "x_13^4 x_2 x_5^2"
--        gmstring = "x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4"
--        hmstring = "x_2^5"
--        fm = fromString fmstring :: M.Monomial RP.Lex 14
--        gm = fromString gmstring :: M.Monomial RP.Lex 5
--        hm = fromString hmstring :: M.Monomial RP.Lex 14
--        im = mempty :: M.Monomial RP.Lex 5
--    putStrLn $ "fm = " ++ (formatSS . show) fm
--    putStrLn $ "gm = " ++ (formatSS . show) gm
--    putStrLn $ "hm = " ++ (formatSS . show) hm
--    putStrLn $ "fm * hm = " ++ (formatSS . show) (fm <> hm)
--    putStrLn $ "gm * im = " ++ (formatSS . show) (gm <> im)
    let fpstring = "-4x_3^4 x_2 x_5^2 - 7/2 x_1^2 x_3 x_2^5 x_9^3 + 6 x_2^4 x_5^3 + 9/3 x_4^9x_3^9x_2^9"
        gpstring = "25x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4 + 7x_3^3 x_2^2 x_5^2 x_8^2 x_7 x_4 + x_5^9x_2^4 x_6^2"
        hpstring = "x_1^5 + 2x_1^7 - 4x_1^8 + x_1 + 35/15 - 2x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4"
        fp = (fromString fpstring) :: P.Polynomial RP.Q RP.Glex 13
        gp = (fromString gpstring) :: P.Polynomial RP.Q RP.Glex 13
        hp = (fromString hpstring) :: P.Polynomial RP.Q RP.Glex 13
        zero = fromString "" :: P.Polynomial RP.Q RP.Glex 13
    putStrLn $ "fp = " ++ (formatSS . show) fp
    putStrLn $ "gp = " ++ (formatSS . show) gp
    putStrLn $ "hp = " ++ (formatSS . show) hp
    print $ P.isZero fp
    print $ P.isZero zero
    putStrLn $ "LT(fp) = " ++ (formatSS . show $ P.leadTerm fp)
    putStrLn $ "LT(gp) = " ++ (formatSS . show $ P.leadTerm gp)
    putStrLn $ "LT(hp) = " ++ (formatSS . show $ P.leadTerm hp)
    putStrLn $ "LM(fp) = " ++ (formatSS . show $ P.leadMonomP fp)
    putStrLn $ "LM(gp) = " ++ (formatSS . show $ P.leadMonomP gp)
    putStrLn $ "LM(hp) = " ++ (formatSS . show $ P.leadMonomP hp)
    putStrLn $ "LC(fp) = " ++ (formatSS . show $ P.leadCoef fp)
    putStrLn $ "LC(gp) = " ++ (formatSS . show $ P.leadCoef gp)
    putStrLn $ "LC(hp) = " ++ (formatSS . show $ P.leadCoef hp)
    putStrLn $ "fp + gp = " ++ (formatSS . show $ fp + gp)
    putStrLn $ "fp + hp = " ++ (formatSS . show $ fp + hp)
    putStrLn $ "gp + hp = " ++ (formatSS . show $ gp + hp)
    print $ P.totalDegree fp
    print $ P.totalDegree gp
    print $ P.totalDegree hp

module Main where

import Data.Char.SScript (formatSS)
import qualified Polynomial as P
import qualified RingParams as RP
import qualified DenseMonom as M
import PolyParsers (Readable(..))

type R = P.Polynomial RP.Q 13 RP.Lex

main :: IO ()
main = do
    let fmstring = "x_13^4 x_2 x_5^2"
        gmstring = "x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4"
        hmstring = "x_2^5"
        fm = fromString fmstring :: M.Monomial 13 RP.Lex
        gm = fromString gmstring :: M.Monomial 13 RP.Lex
        hm = fromString hmstring :: M.Monomial 13 RP.Lex
        im = mempty :: M.Monomial 13 RP.Lex
    let fpstring = "-4x_3^4 x_2 x_5^2 - 5 - 2x_9 - 7/2 x_1^2 x_3 x_2^5 x_9^3 + 6 + 6 x_2^4 x_5^3 - 10/3 + 9/3 x_4^9x_3^9x_2^9"
        gpstring = "25x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4 + 7x_3^3 x_2^2 x_5^2 x_8^2 x_7 x_4 + x_5^9x_2^4 x_6^2"
        hpstring = "x_1^5 + 2x_1^7 + 2x_9 - 4x_1^8 - 4x_1 + x_1 + 35/15 - 2x_1^4 x_3 x_2^7 x_5^2 x_8 x_7^4"
        fp = (fromString fpstring) :: R
        gp = (fromString gpstring) :: R
        hp = (fromString hpstring) :: R
        zero = fromString "5 - 5" :: R
        zero2 = fromInteger 0 :: R
        one1 = fromInteger 1 :: R
        five = fromInteger 5 :: R
        one2 = fromString "5 + 7 - 20 + 9" :: R
    putStrLn $ "fm = " ++ (formatSS . show) fm
    putStrLn $ "gm = " ++ (formatSS . show) gm
    putStrLn $ "hm = " ++ (formatSS . show) hm
    putStrLn $ "fm * hm = " ++ (formatSS . show) (fm <> hm)
    putStrLn $ "gm * im = " ++ (formatSS . show) (gm <> im)
    putStrLn $ "fp = " ++ (formatSS . show) fp
    putStrLn $ "gp = " ++ (formatSS . show) gp
    putStrLn $ "hp = " ++ (formatSS . show) hp
    putStrLn $ "zero = " ++ (formatSS . show) zero
    putStrLn $ "zero2 = " ++ (formatSS . show) zero2
    putStrLn $ "one1 = " ++ (formatSS . show) one1
    putStrLn $ "one2 = " ++ (formatSS . show) one2
    putStrLn $ "five = " ++ (formatSS . show) five
    print $ P.isZero fp
    print $ P.isZero zero
    print $ P.isZero zero2
    print $ P.isZero one1
    print $ one1 == one2
    print $ zero == zero2
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
    putStrLn $ "gp + 0 = " ++ (formatSS . show $ gp + zero)
    print $ P.totalDegree fp
    print $ P.totalDegree gp
    print $ P.totalDegree hp
    print $ P.totalDegree zero
    print $ P.multiDegree fp
    print $ P.multiDegree gp
    print $ P.multiDegree hp
    print $ P.multiDegree zero
    print $ negate fp
    print $ negate gp
    print $ negate hp
    print $ negate zero
    print $ negate one1
    putStrLn $ "fp = " ++ (formatSS . show) fp
    putStrLn $ "gp = " ++ (formatSS . show) gp
    putStrLn $ "hp = " ++ (formatSS . show) hp
    putStrLn $ "fp * gp = " ++ (formatSS . show $ fp * gp)
    putStrLn $ "fp * hp = " ++ (formatSS . show $ fp * hp)
    putStrLn $ "gp * hp = " ++ (formatSS . show $ gp * hp)
    putStrLn $ "gp * 0 = " ++ (formatSS . show $ gp * zero)
    putStrLn $ "gp * 1 = " ++ (formatSS . show $ gp * one1)
    print $ fp * gp == gp * fp
    print $ fp * hp == hp * fp
    print $ gp * hp == hp * gp
    print $ P.isZero (gp * zero)
    print $ (fromInteger 1) * gp == gp && gp == gp * (fromInteger 1)

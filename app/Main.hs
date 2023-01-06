-------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- Driver for polynomial operations and algorithms.
-------------------------------------------------------------------------------
import Data.Char.SScript (formatSS)
import qualified RingParams as RP
import qualified Polynomial as P
import qualified DenseMonom as M
import Algorithms ((//), (/%))
import PolyParsers (Readable(..))

type R = P.Polynomial RP.Q 2 RP.Lex

main :: IO ()
main = do
    let f1string = "x_1x_2^2 + 1"
        f2string = "x_1^2x_2 + x_1x_2^2 + x_2^2"
        f3string = "x_1x_2^2 - x_1"
        g1string = "x_1x_2 + 1"
        g2string = "x_2 + 1"
        g3string = "x_1x_2 - 1"
        g4string = "x_2^2 - 1"
        f1 = (fromString f1string) :: R
        f2 = (fromString f2string) :: R
        f3 = (fromString f3string) :: R
        g1 = (fromString g1string) :: R
        g2 = (fromString g2string) :: R
        g3 = (fromString g3string) :: R
        g4 = (fromString g4string) :: R
--        fmstring = "x_1^3x_2^5x_3"
--        gmstring = "x_1^2x_3^4"
--        fm = (fromString fmstring) :: M.Monomial 3 RP.Lex
--        gm = (fromString gmstring) :: M.Monomial 3 RP.Lex
--    putStrLn $ "fm = " ++ (formatSS . show) fm
--    putStrLn $ "gm = " ++ (formatSS . show) gm
--    putStrLn $ "gcd fm gm = " ++ (formatSS . show) (M.gcdMon fm gm)
--    putStrLn $ "lcm fm gm = " ++ (formatSS . show) (M.lcmMon fm gm)
--    putStrLn $ "Maybe gcd Nothing gm = " ++ (formatSS . show) (M.gcdMonM Nothing (Just gm))
--    putStrLn $ "Maybe gcd fm Nothing = " ++ (formatSS . show) (M.gcdMonM (Just fm) Nothing)
--    putStrLn $ "Maybe gcd fm gm = " ++ (formatSS . show) (M.gcdMonM (Just fm) (Just gm))
--    putStrLn $ "Maybe lcm fm gm = " ++ (formatSS . show) (M.lcmMonM (Just fm) (Just gm))
    putStrLn $ "f1 = " ++ (formatSS . show) f1
    putStrLn $ "f2 = " ++ (formatSS . show) f2
    putStrLn $ "f3 = " ++ (formatSS . show) f3
    putStrLn $ "g1 = " ++ (formatSS . show) g1
    putStrLn $ "g2 = " ++ (formatSS . show) g2
    putStrLn $ "g3 = " ++ (formatSS . show) g3
    putStrLn $ "g4 = " ++ (formatSS . show) g4
    putStrLn $ "f1 /% [g1,g2] = " ++ (formatSS . show) (f1 /% [g1,g2])
    putStrLn $ "f2 /% [g3,g4] = " ++ (formatSS . show) (f2 /% [g3,g4])
    putStrLn $ "f2 /% [g4,g3] = " ++ (formatSS . show) (f2 /% [g4,g3])
    putStrLn $ "f3 /% [g3,g4] = " ++ (formatSS . show) (f3 /% [g3,g4])
    putStrLn $ "f3 /% [g4,g3] = " ++ (formatSS . show) (f3 /% [g4,g3])
    putStrLn $ "f1 // [g1,g2] = " ++ (formatSS . show) (f1 // [g1,g2])
    putStrLn $ "f2 // [g3,g4] = " ++ (formatSS . show) (f2 // [g3,g4])
    putStrLn $ "f2 // [g4,g3] = " ++ (formatSS . show) (f2 // [g4,g3])
    putStrLn $ "f3 // [g3,g4] = " ++ (formatSS . show) (f3 // [g3,g4])
    putStrLn $ "f3 // [g4,g3] = " ++ (formatSS . show) (f3 // [g4,g3])

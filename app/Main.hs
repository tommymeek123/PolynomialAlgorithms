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
        f4string = "x_1^2x_2^2 - x_1^3x_2"
        g1string = "x_1x_2 + 1"
        g2string = "x_2 + 1"
        g3string = "x_1x_2 - 1"
        g4string = "x_2^2 - 1"
        f1 = (fromString f1string) :: R
        f2 = (fromString f2string) :: R
        f3 = (fromString f3string) :: R
        f4 = (fromString f4string) :: R
        g1 = (fromString g1string) :: R
        g2 = (fromString g2string) :: R
        g3 = (fromString g3string) :: R
        g4 = (fromString g4string) :: R
        fmstring = "x_1x_2^2"
        gmstring = "x_1^2x_2"
        fm = (fromString fmstring) :: M.Monomial 2 RP.Lex
        gm = (fromString gmstring) :: M.Monomial 2 RP.Lex
    putStrLn $ "fm = " ++ (formatSS . show) fm
    putStrLn $ "gm = " ++ (formatSS . show) gm
    putStrLn $ "f1 = " ++ (formatSS . show) f1
    putStrLn $ "f2 = " ++ (formatSS . show) f2
    putStrLn $ "f3 = " ++ (formatSS . show) f3
    putStrLn $ "f4 = " ++ (formatSS . show) f4
    putStrLn $ "g1 = " ++ (formatSS . show) g1
    putStrLn $ "g2 = " ++ (formatSS . show) g2
    putStrLn $ "g3 = " ++ (formatSS . show) g3
    putStrLn $ "g4 = " ++ (formatSS . show) g4
    putStrLn $ "f4 / fm = " ++ (formatSS . show) (f4 `P.tryDivideByMon` fm)
    putStrLn $ "f4 / gm = " ++ (formatSS . show) (f4 `P.tryDivideByMon` gm)
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

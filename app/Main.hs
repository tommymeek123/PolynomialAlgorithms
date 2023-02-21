-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- Driver for polynomial operations and algorithms.
-----------------------------------------------------------------------------------------
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Char.SScript (formatSS)
import qualified RingParams as RP
import qualified Polynomial as P
import Algorithms ((//), (/%), gb, isBasisOf, isGB)
import PolyParsers (Readable(..))

type R = P.Polynomial RP.Q 2 RP.GLex
type S = P.Polynomial RP.Q 4 RP.GRevLex
type T = P.Polynomial RP.Q 5 RP.Lex
type U = P.Polynomial RP.Q 3 RP.GLex

main :: IO ()
main = do
    let --f1string = "x_1^3 - 2x_1x_2"
--        f2string = "x_1^2x_2 - 2x_2^2 + x_1"
--        g1string = "x_1^6 - x_2x_3^4x_4"
--        g2string = "x_1x_2^4 - x_3^5"
--        g3string = "x_1^5x_3 - x_2^5x_4"
--        f1 = (fromString f1string) :: R
--        f2 = (fromString f2string) :: R
--        g1 = (fromString g1string) :: S
--        g2 = (fromString g2string) :: S
--        g3 = (fromString g3string) :: S
        f1 = P.fromMap $ Map.fromList [([3,0],"1"), ([1,1],"-2")] :: R
        f2 = P.fromMap $ Map.fromList [([2,1],"1"), ([0,2],"-2"), ([1,0],"1")] :: R
        f3 = P.fromMap $ Map.fromList [([2,0],"-1")] :: R
        f4 = P.fromMap $ Map.fromList [([1,1],"-2")] :: R
        f5 = P.fromMap $ Map.fromList [([0,2],"-2"), ([1,0],"1")] :: R
        g1 = P.fromMap $ Map.fromList [([6,0,0,0],"1"), ([0,1,4,1],"-1")] :: S
        g2 = P.fromMap $ Map.fromList [([1,4,0,0],"1"), ([0,0,5,0],"-1")] :: S
        g3 = P.fromMap $ Map.fromList [([5,0,1,0],"1"), ([0,5,0,1],"-1")] :: S
        g = P.fromMap $ Map.fromList [([0,0,26,0],"1"), ([0,25,0,1],"-1")] :: S
        h1 = P.fromMap $ Map.fromList [([0,0,1,0,0],"1"), ([1,0,0,0,0],"-1"), ([0,1,0,0,0],"-1")] :: T
        h2 = P.fromMap $ Map.fromList [([0,0,0,1,0],"1"), ([2,0,0,0,0],"-1"), ([1,1,0,0,0],"-2")] :: T
        h3 = P.fromMap $ Map.fromList [([0,0,0,0,1],"1"), ([3,0,0,0,0],"-1"), ([2,1,0,0,0],"-3")] :: T
        i1 = P.fromMap $ Map.fromList [([5,0,0],"1"), ([0,4,0],"1"), ([0,0,3],"1"), ([0,0,0],"-1")] :: U
        i2 = P.fromMap $ Map.fromList [([3,0,0],"1"), ([0,3,0],"1"), ([0,0,2],"1"), ([0,0,0],"-1")] :: U
        fs = [f1,f2,f3,f4,f5]
        gbf = gb [f1,f2]
        gbg = gb [g1,g2,g3]
        gbh = gb [h1,h2,h3]
        gbi = gb [i1,i2]
--    putStrLn $ "f1 = " ++ (formatSS . show) f1
--    putStrLn $ "f2 = " ++ (formatSS . show) f2
--    putStrLn $ "f3 = " ++ (formatSS . show) f3
--    putStrLn $ "f4 = " ++ (formatSS . show) f4
--    putStrLn $ "f5 = " ++ (formatSS . show) f5
--    putStrLn $ "g1 = " ++ (formatSS . show) g1
--    putStrLn $ "g2 = " ++ (formatSS . show) g2
--    putStrLn $ "g3 = " ++ (formatSS . show) g3
--    putStrLn $ "h1 = " ++ (formatSS . show) h1
--    putStrLn $ "h2 = " ++ (formatSS . show) h2
--    putStrLn $ "h3 = " ++ (formatSS . show) h3
--    putStrLn $ "redh1 = " ++ (formatSS . show) (P.normalize h1)
--    putStrLn $ "GB <h1,h2, h3> = " ++ (formatSS . show) gbh
--    putStrLn $ "GB <f1,f2> = " ++ (formatSS . show) gbf
--    putStrLn $ "GB <g1,g2,g3> = " ++ (formatSS . show) gbg
    print $ gbf `isBasisOf` [f1,f2]
    print $ isGB gbf
    print $ gbg `isBasisOf` [g1,g2,g3]
    print $ isGB gbg
    print $ gbh `isBasisOf` [h1,h2,h3]
    print $ isGB gbh
    print $ gbi `isBasisOf` [i1,i2]
    print $ isGB gbi
--    print $ (and . map (`elem` gbf)) fs
--    print $ g `elem` gbg
--    putStrLn $ "GB <h1,h2,h3> = " ++ (formatSS . show) gbh
--    print $ length gbi
    print $ map P.numTerms gbi
    print $ map P.totalDegree gbi
    putStrLn $ "GB <i1,i2> = " ++ (formatSS . show) gbi

--main :: IO ()
--main = do
--    let f1string = "x_1x_2^2 + 1"
--        f2string = "x_1^2x_2 + x_1x_2^2 + x_2^2"
--        f3string = "x_1x_2^2 - x_1"
--        f4string = "x_1^2x_2^2 - x_1^3x_2"
--        g1string = "x_1x_2 + 1"
--        g2string = "x_2 + 1"
--        g3string = "x_1x_2 - 1"
--        g4string = "x_2^2 - 1"
--        fstring = "x_1^3x_2^2 - x_1^2x_2^3 + x_1"
--        gstring = "3x_1^4x_2 + x_2^2"
--        fmstring = "x_1x_2^2"
--        gmstring = "1"
--        fm = (fromString fmstring) :: M.Monomial 2 RP.Lex
--        gm = (fromString gmstring) :: M.Monomial 2 RP.Lex
--        f1 = (fromString f1string) :: R
--        f2 = (fromString f2string) :: R
--        f3 = (fromString f3string) :: R
--        f4 = (fromString f4string) :: R
--        g1 = (fromString g1string) :: R
--        g2 = (fromString g2string) :: R
--        g3 = (fromString g3string) :: R
--        g4 = (fromString g4string) :: R
--        f = (fromString fstring) :: P.Polynomial RP.Q 2 RP.Glex
--        g = (fromString gstring) :: P.Polynomial RP.Q 2 RP.Glex
--    putStrLn $ "fm = " ++ (formatSS . show) fm
--    putStrLn $ "gm = " ++ (formatSS . show) gm
--    putStrLn $ "f1 = " ++ (formatSS . show) f1
--    putStrLn $ "f2 = " ++ (formatSS . show) f2
--    putStrLn $ "f3 = " ++ (formatSS . show) f3
--    putStrLn $ "f4 = " ++ (formatSS . show) f4
--    putStrLn $ "g1 = " ++ (formatSS . show) g1
--    putStrLn $ "g2 = " ++ (formatSS . show) g2
--    putStrLn $ "g3 = " ++ (formatSS . show) g3
--    putStrLn $ "g4 = " ++ (formatSS . show) g4
--    putStrLn $ "f = " ++ (formatSS . show) f
--    putStrLn $ "g = " ++ (formatSS . show) g
--    putStrLn $ "f1 /% [g1,g2] = " ++ (formatSS . show) (f1 /% [g1,g2])
--    putStrLn $ "f2 /% [g3,g4] = " ++ (formatSS . show) (f2 /% [g3,g4])
--    putStrLn $ "f2 /% [g4,g3] = " ++ (formatSS . show) (f2 /% [g4,g3])
--    putStrLn $ "f3 /% [g3,g4] = " ++ (formatSS . show) (f3 /% [g3,g4])
--    putStrLn $ "f3 /% [g4,g3] = " ++ (formatSS . show) (f3 /% [g4,g3])
--    putStrLn $ "f1 // [g1,g2] = " ++ (formatSS . show) (f1 // [g1,g2])
--    putStrLn $ "f2 // [g3,g4] = " ++ (formatSS . show) (f2 // [g3,g4])
--    putStrLn $ "f2 // [g4,g3] = " ++ (formatSS . show) (f2 // [g4,g3])
--    putStrLn $ "f3 // [g3,g4] = " ++ (formatSS . show) (f3 // [g3,g4])
--    putStrLn $ "f3 // [g4,g3] = " ++ (formatSS . show) (f3 // [g4,g3])
--    putStrLn $ "S(f,g) = " ++ (formatSS . show) (P.sPoly f g)

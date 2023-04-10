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

    print $ gbf `isBasisOf` [f1,f2]
    print $ isGB gbf
    print $ gbg `isBasisOf` [g1,g2,g3]
    print $ isGB gbg
    print $ gbh `isBasisOf` [h1,h2,h3]
    print $ isGB gbh
    print $ gbi `isBasisOf` [i1,i2]
    print $ isGB gbi

    print $ map P.numTerms gbi
    print $ map P.totalDegree gbi

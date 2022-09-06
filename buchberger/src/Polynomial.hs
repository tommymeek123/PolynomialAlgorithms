module Polynomial (Poly(..)
                 , isZero
                 , leadMonom
                 , leadCoef
                 , fromString) where

import Data.List
import Data.List.Split
import Data.Char
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified Base
import DenseMonom as Monom
import Debug.Trace

type Field = Base.Q
baseFromString = Base.ratFromString

data Poly = Poly { monMap :: Map.Map Mon Field } deriving (Eq)

isZero :: Poly -> Bool
isZero = Map.null . monMap

leadMonom :: Poly -> Maybe Mon
leadMonom = fmap fst . Map.lookupMax . monMap

leadCoef :: Poly -> Maybe Field
leadCoef = fmap snd . Map.lookupMax . monMap

--leadTerm :: Poly -> Maybe Poly
--
--totalPolyDegree :: Poly -> Maybe Int
--
--polyAdd :: Poly -> Poly -> Poly
--
--polyMult :: Poly -> Poly -> Poly

instance Show Poly where
    show = format

format :: Poly -> String
format = intercalate " + "
        . map (\(k,v) -> show v ++ show k)
        . Map.assocs
        . monMap

mapFromString :: MonOrder -> Int -> String -> Map.Map Mon Field
mapFromString o n = Map.fromList
                        . map (\(ks,vs) -> (monFromString o n ks, baseFromString vs))
                        . map (swap . break (=='x'))
                        . filter (not . null)
                        . splitOn "+"
                        . intercalate "+-"
                        . splitOn "-"
                        . filter (not . isSpace)

fromString :: MonOrder -> Int -> String -> Poly
fromString o n s = Poly $ mapFromString o n s

--format :: Poly -> String

--leadMonom :: Poly -> Maybe Mon
--leadMonom p
--    | isZero p = Nothing
--    | otherwise = Just $ (Map.findMax . monMap) p

--leadCoef :: Poly -> Maybe Q
--leadCoef p = leadMonom >>= flip lookup (monMap p)

--leadTerm :: Poly -> Poly
--leadTerm p
--    | Map.null (monMap p) = Poly Map.empty
--    | otherwise = (Poly . uncurry Map.singleton . Map.findMax . monMap) p

--totalPolyDegree :: Poly -> Maybe Int
--totalPolyDegree p
--    | Map.null (monMap p) = Nothing
--    | otherwise = Just $ (totalMonDeg . head . Map.keys . monMap . leadTerm) p

--polyAdd :: Poly -> Poly -> Poly
-- polyAdd = Map.unionWith (+)

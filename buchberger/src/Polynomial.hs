module Polynomial (Poly(..)
                 , isZero
                 , leadMonom
                 , leadCoef
                 , format
                 , fromString) where

import Data.List
import Data.List.Split
import Data.Char.SScript (formatSS)
import Data.Char
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified BaseRing
import qualified DenseMonom as Monom
import Debug.Trace
import RingParams

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
    show = intercalate " + "
          . map (\(k,v) -> BaseRing.show v ++ Monom.show k)
          . Map.assocs
          . monMap

format :: Poly -> String
format = formatSS . show

mapFromString :: RingParams -> String -> Map.Map Mon Field
mapFromString r = Map.fromList
                 . map (\(ks,vs) -> (Monom.fromString r ks, BaseRing.fromString r vs))
                 . map (swap . break (=='x'))
                 . filter (not . null)
                 . splitOn "+"
                 . intercalate "+-"
                 . splitOn "-"
                 . filter (not . isSpace)

fromString :: RingParams -> String -> Poly
fromString r s = Poly $ mapFromString r s

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

module Polynomial ( Poly(..)
                  , isZero
                  , leadMonom
                  , leadCoef
                  , format
                  , fromString
                  ) where

import Data.List
import Data.List.Split
import Data.Char.SScript (formatSS)
import Data.Char (digitToInt, isSpace)
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified BaseRing as BR
import qualified DenseMonom as M
import qualified RingParams as RP
import Debug.Trace

data Poly = Poly { monMap :: Map.Map M.Mon BR.Field } deriving (Eq)

isZero :: Poly -> Bool
isZero = Map.null . monMap

leadMonom :: Poly -> Maybe M.Mon
leadMonom = fmap fst . Map.lookupMax . monMap

leadCoef :: Poly -> Maybe BR.Field
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
         . map (\(k,v) -> show v ++ show k)
         . Map.assocs
         . monMap

format :: Poly -> String
format = formatSS . show

mapFromString :: RP.RingParams -> String -> Map.Map M.Mon BR.Field
mapFromString r = Map.fromList
                . map (\(ks,vs) -> (M.fromString r ks, BR.fromString r vs))
                . map (swap . break (=='x'))
                . splitOn "+" -- Make a list of terms
                . intercalate "+-"
                . filter (not . null)
                . splitOn "-" -- This handles subtraction
                . filter (not . isSpace)

fromString :: RP.RingParams -> String -> Poly
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

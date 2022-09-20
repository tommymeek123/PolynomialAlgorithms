module Polynomial ( Poly(..)
                  , format
                  , fromString
                  , isZero
                  , leadCoef
                  , leadMonom
                  , leadMonomF
                  ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Char.SScript (formatSS)
import Data.Char (digitToInt, isSpace)
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified BaseRing as BR
import qualified DenseMonom as M
import qualified RingParams as RP

data Poly = Poly { monMap :: Map.Map M.Mon BR.Field } deriving (Eq)

instance Show Poly where
    show = intercalate " + "
         . map (\(k,v) -> show v ++ show k)
         . Map.assocs
         . monMap

format :: Poly -> String
format = formatSS . show

fromString :: RP.RingParams -> String -> Poly
fromString r s = Poly $ mapFromString r s

isZero :: Poly -> Bool
isZero = Map.null . monMap

leadMonom :: Poly -> Maybe M.Mon
leadMonom = fmap fst . Map.lookupMax . monMap

leadMonomF :: Poly -> String
leadMonomF = formatSS . show . leadMonom

leadCoef :: Poly -> Maybe BR.Field
leadCoef = fmap snd . Map.lookupMax . monMap

--leadTerm :: Poly -> Maybe Poly

--totalPolyDegree :: Poly -> Maybe Int

--add :: Poly -> Poly -> Poly

--mult :: Poly -> Poly -> Poly

mapFromString :: RP.RingParams -> String -> Map.Map M.Mon BR.Field
mapFromString r = Map.fromList
                . map (\(ks,vs) -> (M.fromString r ks, BR.fromString r vs))
                . map (swap . break (=='x'))
                . splitOn "+" -- Make a list of terms
                . intercalate "+-"
                . filter (not . null)
                . splitOn "-" -- This handles subtraction
                . filter (not . isSpace)

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

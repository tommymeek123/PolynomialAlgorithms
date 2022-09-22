module Polynomial ( Poly(..)
                  , add
                  , fromString
                  , isZero
                  , leadCoef
                  , leadMonom
                  , leadMonomP
                  , leadTerm
                  , totalDegree
                  ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Char (digitToInt, isSpace)
import Data.Tuple (swap)
import Data.Maybe (isNothing)
import Control.Monad (liftM2)
import Data.Composition ((.:))
import Data.Foldable (maximumBy)
import qualified Data.Map as Map
import qualified BaseRing as R
import qualified DenseMonom as M
import qualified RingParams as RP

data Poly = Poly { monMap :: Map.Map M.Mon R.Field } deriving (Eq)

instance Show Poly where
    show = let removeOnes s = if head s == '1' then tail s else s
           in intercalate " + "
            . map removeOnes
            . map (\(k,v) -> show v ++ show k)
            . Map.assocs
            . monMap

fromString :: RP.RingParams -> String -> Poly
fromString r s = Poly $ mapFromString r s

isZero :: Poly -> Bool
isZero = Map.null . monMap

leadMonom :: Poly -> Maybe M.Mon
leadMonom = fmap fst . Map.lookupMax . monMap

leadMonomP :: Poly -> Maybe Poly
leadMonomP p = case leadMonom p of
    Just m -> fmap Just Poly $ Map.singleton m 1
    Nothing -> Nothing

leadCoef :: Poly -> Maybe R.Field
leadCoef = fmap snd . Map.lookupMax . monMap

leadTerm :: Poly -> Maybe Poly
leadTerm p = liftM2 (Poly .: Map.singleton) (leadMonom p) (leadCoef p)

--totalDegree :: Poly -> Maybe Int
--totalDegree p = M.totalDeg . fst $ maximumBy f (monMap p)
--    where f x y = compare ((M.totalDeg . fst) x) ((M.totalDeg . fst) y)

totalDegree :: Poly -> Maybe Int
totalDegree p = if isZero p
                then Nothing
                else Just $ M.totalDeg . maximumBy f . Map.keys . monMap $ p
                where f x y = compare (M.totalDeg x) (M.totalDeg y)

add :: Poly -> Poly -> Poly
add p q = Poly $ Map.unionWith (+) (monMap p) (monMap q)

--mult :: Poly -> Poly -> Poly

mapFromString :: RP.RingParams -> String -> Map.Map M.Mon R.Field
mapFromString r = Map.fromList
                . map (\(ks,vs) -> (M.fromString r ks, R.fromString r vs))
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

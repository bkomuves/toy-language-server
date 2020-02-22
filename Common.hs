
-- | Common types etc

{-# LANGUAGE BangPatterns, DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
module Common where

--------------------------------------------------------------------------------

import Data.List
import Data.Ord
import qualified Data.Map as Map

import GHC.Generics (Generic)
import Control.DeepSeq

import Text.Megaparsec.Pos 

--------------------------------------------------------------------------------
-- * names

quoted :: String -> String
quoted s = '`' : s ++ "`"

--------------------------------------------------------------------------------
-- * locations

-- megaparsec is way too over-engineered...
data SrcPos 
  = SrcPos !Int !Int
  deriving (Eq,Ord,Show,Generic,NFData)
  
toSrcPos :: SourcePos -> SrcPos
toSrcPos (SourcePos fn line col) = SrcPos (unPos line) (unPos col)

data Location
  = Location !SrcPos !SrcPos
  deriving (Eq,Ord,Show,Generic,NFData)

isInside :: SrcPos -> Location -> Bool
isInside pos (Location a b) = pos >= a && pos < b

locSpan :: Location -> Location -> Location
locSpan (Location a1 b1) (Location a2 b2) = Location (min a1 a2) (max b1 b2)

data Located a 
  = Located !Location !a  
  deriving (Show,Functor,Generic,NFData)

location :: Located a -> Location
location (Located loc _) = loc

forgetLocation :: Located a -> a
forgetLocation (Located _ y) = y

class ShowF f where
  showF :: Show a => f a -> String
  
instance ShowF Located where
  showF = show

--------------------------------------------------------------------------------

findInnerMost :: SrcPos -> Map.Map Location a -> Maybe (Location,a)
findInnerMost pos list = findInnerMostList pos (Map.toList list)
 
findInnerMostList :: SrcPos -> [(Location,a)] -> Maybe (Location,a)
findInnerMostList pos list = 
  case filter (\(loc,_) -> isInside pos loc) list of
    []        -> Nothing
    [(loc,y)] -> Just (loc,y)
    pairs     -> Just $ head $ sortBy cmpLocPair pairs
  where
    cmpLocPair (loc1,y1) (loc2,y2) = cmpLoc loc1 loc2
    cmpLoc (Location p1 q1) (Location p2 q2) =
      case compare p1 p2 of
        GT -> LT
        LT -> GT
        EQ -> compare q1 q2     

{-      
reverseCompare x y = case compare x y of
  LT -> GT
  EQ -> EQ
  GT -> LT

reverseComparing f x y = reverseCompare (f x) (f y)
-}

--------------------------------------------------------------------------------
        
 
import System.Environment (getArgs)
import System.Random (getStdGen, randomR, randomRs, randomRIO, StdGen)
import Data.List (sortOn, mapAccumL)
import Data.Tuple (swap)
import Control.Monad (liftM)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map


type Position = (Int,Int)
type PossibleMoveMap = (Map Position (Set Position))
data KnightPath = Path {- Path -} [Position]    {- The available moves from a given position -} PossibleMoveMap deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    let initialPosition = read . head $ args
    let initialPaths = [positionToPath initialPosition]
    randGen <- getStdGen

    let allPaths = iterate (expandPaths randGen) initialPaths
    let allSolutions = allPaths !! 60

    sequence_ . map (putStrLn . show . reverse) . take 1 . map getPath $ allSolutions

getPath :: KnightPath -> [Position]
getPath (Path p _) = p

allBoardPositions :: [Position]
allBoardPositions = [(x, y) | x <- [1..8], y <- [1..8] ]

initialMoveMap :: Map Position (Set Position)
initialMoveMap = foldl insertPossibleMoves Map.empty allBoardPositions
    where insertPossibleMoves moveMap position = Map.insert position (possibleMoves position) moveMap

positionToPath :: Position -> KnightPath
positionToPath p = Path [p] (removePosition p initialMoveMap)

removePosition :: Position -> PossibleMoveMap -> PossibleMoveMap
removePosition p m = Map.map adjustMap m
    where adjustMap moveSet = Set.delete p moveSet

possibleMoves :: Position -> Set Position
possibleMoves (x,y) = Set.fromList . filter (\(i,j) -> i >= 1 && i <= 8 && j >=1 && j <=8) $ pm
    where pm = [(x+2, y+1), 
                (x+2, y-1), 
                (x-2, y+1), 
                (x-2, y-1), 
                (x+1, y+2), 
                (x+1, y-2),
                (x-1, y+2), 
                (x-1, y-2)]

-- Make a move - resulting in 1 or more possible paths
expandPath :: KnightPath -> [KnightPath]
expandPath (Path path@(x:_) possibleMoveMap) = map newPath nextMoves
    where newPath np = Path (np:path) (removePosition np possibleMoveMap)
          nextMoves = Set.toList $ possibleMoveMap ! x

expandPaths :: StdGen -> [KnightPath] -> [KnightPath]
expandPaths randGen paths = concatMap (randomSortBySize randGen . expandPath) paths

randomSortBySize :: StdGen -> [KnightPath] -> [KnightPath]

randomSortBySize randGen paths = let
    positionPriority (Path (p:_) moveMap) = (Set.size $ moveMap ! p) ^ (2 :: Int)
    randomizedPriorities = snd . mapAccumL (\gen path -> swap $ randomR (0, positionPriority path) gen) randGen $ paths
    sortedPaths = map fst . sortOn snd $ zip paths randomizedPriorities
    in sortedPaths
    

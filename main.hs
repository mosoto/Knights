import Data.List (sortOn)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map


type Position = (Int,Int)
type PossibleMoveMap = (Map Position (Set Position))
data KnightPath = Path {- Path -} [Position]    {- The available moves from a given position -} PossibleMoveMap deriving (Show)

main :: IO ()
main = do
    let initialPosition = (1,1)
    let initialPaths = [positionToPath initialPosition]
    let expandPaths = concatMap expandPath
    let allPaths = iterate expandPaths initialPaths
    let allSolutions = allPaths !! 63

    sequence_ . map (putStrLn . show . reverse) . take 10 . map getPath $ allSolutions

getPath :: KnightPath -> [Position]
getPath (Path p _) = p

allBoardPositions :: [Position]
allBoardPositions = [(x, y) | x <- [1..8], y <- [1..8] ]

initialMoveMap :: Map Position (Set Position)
initialMoveMap = foldl insertPossibleMoves Map.empty allBoardPositions
    where insertPossibleMoves map position = Map.insert position (possibleMoves position) map

positionToPath :: Position -> KnightPath
positionToPath p = Path [p] (removePosition p initialMoveMap)

removePosition :: Position -> PossibleMoveMap -> PossibleMoveMap
removePosition p m = Map.map adjustMap m
    where adjustMap moveSet = Set.delete p moveSet

possibleMoves :: Position -> Set Position
possibleMoves (x,y) = Set.fromList . filter (\(x,y) -> x >= 1 && x <= 8 && y >=1 && y <=8) $ pm
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
expandPath (Path path@(x:xs) possibleMoveMap) = map newPath sortedNextMoves
    where newPath np = Path (np:path) (removePosition np possibleMoveMap)
          nextMoves = Set.toList $ possibleMoveMap ! x
          sortedNextMoves = sortOn (\p -> Set.size $ possibleMoveMap ! p) nextMoves

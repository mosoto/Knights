import Control.Applicative
import Data.Array
import Data.List (nub)
import Data.Set (Set, singleton, difference, empty, insert, delete, toList, fromList, size)

type Position = (Int,Int)
data KnightPath = Path [Position] {- The used positions -} (Set Position)

main :: IO ()
main = do
    let allPositions = [(x::Int, y::Int) | x <- [1..8], y <- [1..8] ]
    let moveMap = array ((1,1), (8,8)) . map (\p -> (p, possibleMovesSet p)) $ allPositions
    let startPaths = map (\a -> Path [a] (singleton a)) allPositions
    let nextMoves (x:xs) used = difference (moveMap ! x) used
    let expandPath = concatMap (\(Path path' used) -> map (\nm -> Path (nm:path') (insert nm used)) (toList $ nextMoves path' used))
    let allPaths = filter (\(Path p u) -> (size u) == 64 ) ((iterate expandPath startPaths) !! 63)

    print $ map getPath $ take 1 $ allPaths


getPath (Path p up) = p

possibleMovesSet :: (Ord i, Num i) => (i,i) -> Set (i,i)
possibleMovesSet p = fromList (possibleMoves p)

possibleMoves :: (Ord i, Num i) => (i,i) -> [(i,i)]
possibleMoves (x,y) = filter (\(x,y) -> x >= 1 && x <= 8 && y >=1 && y <=8) pm
    where pm = [(x+2, y+1), 
                (x+2, y-1), 
                (x-2, y+1), 
                (x-2, y-1), 
                (x+1, y+2), 
                (x+1, y-2),
                (x-1, y+2), 
                (x-1, y-2)]

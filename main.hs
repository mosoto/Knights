import Control.Applicative
import Data.Array

main :: IO ()
main = do
    let allPositions = [(x::Int, y::Int) | x <- [1..8], y <- [1..8] ]
    let moveMap = array ((1,1), (8,8)) . map (\p -> (p, possibleMoves p)) $ allPositions
    let startPaths = map (\a -> [a]) allPositions
    print $ take 10 startPaths


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

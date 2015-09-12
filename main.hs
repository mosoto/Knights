
import System.Random (randomIO, randomRIO)
import Data.Foldable (foldlM)
import Data.Array
import System.Random.Shuffle (shuffleM)
import Data.Set (Set, fromList, member)
import Debug.Trace (trace)

type Position = (Int,Int)
type KnightPath = Array Int Position

main :: IO()
main = do
    let allPositions = [(x, y) :: Position | x <- [1..8], y <- [1..8] ]
    let moveMap = array ((1,1), (8,8)) . map (\p -> (p, possibleMovesSet p)) $ allPositions
    let energyOfState = energy moveMap :: EnergyOfState KnightPath
    shuffledPositions <- shuffleM allPositions
    let startState = listArray (0,63) shuffledPositions
    let maxK = 20000000
    annealedState <- simulatedAnnealing startState maxK neighbour temperature energyOfState acceptanceProb
    print $ energyOfState startState
    print $ energyOfState annealedState
    print $ annealedState



temperature :: TemperatureSchedule
temperature p = t
    where t = 2 * (1 - p)

acceptanceProb :: AcceptanceProbability
acceptanceProb es es' t = if es' < es then 1 else exp (- (es' - es) / t)

-- swaps two consecutive positions in the path
neighbour :: NeighbourMove KnightPath
neighbour path = do
    let lowerBound = (fst . bounds $ path)
    let upperBound = (snd . bounds $ path)
    randIndex1 <- randomRIO (lowerBound, upperBound)
    randIndex2 <- randomRIO (lowerBound, upperBound)
    let newPath = swap path randIndex1 randIndex2
    return newPath
    
swap :: (Ix i) => Array i e -> i -> i -> Array i e
swap arr i1 i2 = arr // [(i1, arr ! i2), (i2, arr ! i1)]

energy :: Array Position (Set Position) -> KnightPath -> Double
energy moveMap knightPath = fromIntegral . length . filter not $ moveValidity
    where pathArr = elems knightPath
          moveValidity = zipWith (\from to -> member to (moveMap ! from)) pathArr (tail pathArr)



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

-----------------------------
-- Simulated Annealing
-----------------------------

type TemperatureSchedule = Double -> Double
type NeighbourMove s = s -> IO s
type EnergyOfState s = s -> Double
-- AcceptanceProbability = EnergyS -> EnergyS' -> Temperature -> ProbabilityOfAccept
type AcceptanceProbability = Double -> Double -> Double -> Double

-- initial state -> maxSteps -> move function -> temperatureSchedule -> energy of state -> AcceptanceProb -> final state
simulatedAnnealing :: s -> Int -> NeighbourMove s -> TemperatureSchedule -> EnergyOfState s -> AcceptanceProbability -> IO s
simulatedAnnealing s0 maxK move tempSched energy acceptanceProb = do
    let k = [0..(maxK - 1)]
    let nextState state k = do
        let temp = tempSched ((fromIntegral k)  / (fromIntegral maxK))
        state' <- move state
        let energyS = energy state
        let energyS' = energy state'
        let probAccept = acceptanceProb energyS energyS' temp 
        randomNum <- randomIO :: IO Double
        let nextState = if probAccept >= randomNum then state' else state
        return nextState
    foldlM nextState s0 k 
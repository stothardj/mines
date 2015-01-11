module Mines(Dimensions(..),
             Location(..),
             GameState(..),
             Difficulty(..),
             UserAction(..),
             gameStart,
             isLoss,
             isGameOver,
             allLocations,
             numNeighbors,
             flag,
             reveal,
             isOnBoard) where

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Control.Applicative
import Control.Monad.Random
import System.Random.Shuffle

data Dimensions = Dimensions { numRows :: Int, numCols :: Int}
data Location = Location { row :: Int, col :: Int}
              deriving (Eq,Ord,Show)
data GameState = GameState { gameMines :: Set Location,
                             gameFlags :: Set Location,
                             gameRevealed :: Set Location,
                             gameDimensions :: Dimensions }
data Difficulty = Easy | Medium | Hard
data UserAction = RevealLocation | FlagLocation | Quit
                deriving Show

allLocations :: Dimensions -> [Location]
allLocations dim = Location <$> [1..numRows dim] <*> [1..numCols dim]

mineLocations :: MonadRandom m => Dimensions -> Int -> m (Set Location)
mineLocations dim numMines = do
  mines <- shuffleM $ allLocations dim
  return $ Set.fromList $ take numMines mines

dimensionsForDifficulty :: Difficulty -> Dimensions
dimensionsForDifficulty diff = case diff of
                                Easy -> Dimensions 6 7
                                Medium -> Dimensions 12 10
                                Hard -> Dimensions 15 15

numMinesForDifficulty :: MonadRandom m => Difficulty -> m (Int)
numMinesForDifficulty diff = case diff of
                              Easy -> getRandomR (3,6)
                              Medium -> getRandomR (4,9)
                              Hard -> getRandomR (5,15)

mineLocationsForDifficulty :: MonadRandom m => Difficulty -> m (Set Location)
mineLocationsForDifficulty diff = do
  numMines <- numMinesForDifficulty diff
  mineLocations (dimensionsForDifficulty diff) numMines

neighbors :: Location -> [Location]
neighbors loc = [Location (row loc + rd) (col loc + cd) | rd <- [-1..1], cd <- [-1..1], rd /= 0 || cd /= 0]

tally :: Ord k => k -> Map k Int -> Map k Int
tally = Map.alter f
  where
    f Nothing = Just 1
    f (Just x) = Just (x + 1)

numNeighbors :: Set Location -> Map Location Int
numNeighbors = Set.fold f Map.empty
  where
    f loc m = foldl (flip tally) m (neighbors loc)


floodfill :: (Location -> Bool) -> Set Location -> Location -> Set Location
floodfill isBlocking revealed loc
  | loc `Set.member` revealed = revealed
  | isBlocking loc = loc `Set.insert` revealed
  | otherwise = foldl f (loc `Set.insert` revealed) (neighbors loc)
  where f = floodfill isBlocking

isOnBoard :: Dimensions -> Location -> Bool
isOnBoard dim loc = r >= 1 && c >= 1 && r <= nRows && c <= nCols
  where r = row loc
        c = col loc
        nRows = numRows dim
        nCols = numCols dim

hasNeighbors :: Set Location  -> Location -> Bool
hasNeighbors mines loc = loc `Map.member` numNeighbors mines

-- Result of revealing an already revealed location is to reveal the trivially deduced neighbors.
-- This must assume that the flagged locations are correct, otherwise it would provide a means to
-- check the correctness of flags.
-- The logic is, if the number of neighboring mines in the location is equal to the number of
-- neighboring flags, then reveal all other neighboring locations.
revealTrivial :: GameState -> Location -> Set Location
revealTrivial st loc
  | Set.size neighborMines == Set.size neighborFlags = gameRevealed $ Set.foldl reveal st neighborUnrevealed
  | otherwise = revealed
  where revealed = gameRevealed st
        nl = Set.fromList $ neighbors loc
        neighborMines = gameMines st `Set.intersection` nl
        neighborFlags = gameFlags st `Set.intersection` nl
        neighborUnrevealed = nl `Set.difference` gameRevealed st `Set.difference` neighborFlags

reveal' :: GameState -> Location -> Set Location
reveal' st loc
  | loc `Set.member` mines = loc `Set.insert` revealed
  | loc `Set.member` flags = revealed
  | loc `Set.member` revealed = revealTrivial st loc
  | otherwise = floodfill (\l -> (hasNeighbors mines l) || (not (isOnBoard dim l))) revealed loc
  where dim = gameDimensions st
        mines = gameMines st
        flags = gameFlags st
        revealed = gameRevealed st

reveal :: GameState -> Location -> GameState
reveal st loc = st { gameRevealed = reveal' st loc }

flag' :: GameState -> Location -> Set Location
flag' st loc
  | loc `Set.member` flags = loc `Set.delete` flags
  | loc `Set.member` revealed = flags
  | isOnBoard dim loc = loc `Set.insert` flags
  | otherwise = flags
  where dim = gameDimensions st
        flags = gameFlags st
        revealed = gameRevealed st

flag :: GameState -> Location -> GameState
flag st loc = st { gameFlags = flag' st loc}

gameStart :: Difficulty -> IO GameState
gameStart diff = do
  g <- getStdGen
  mines <- evalRandT (mineLocationsForDifficulty diff) g
  return GameState {gameMines = mines,
                    gameFlags = Set.empty,
                    gameRevealed = Set.empty,
                    gameDimensions = dimensionsForDifficulty diff}

isWin :: GameState -> Bool
isWin st = Set.null $ allLocs `Set.difference` correctFlags `Set.difference` rev
  where rev = gameRevealed st
        mines = gameMines st
        flags = gameFlags st
        dim = gameDimensions st
        correctFlags = flags `Set.intersection` mines
        allLocs = Set.fromList $ allLocations dim

isLoss :: UserAction -> GameState -> Bool
isLoss Quit _ = True
isLoss _ st = not . Set.null $ triggeredMines
  where rev = gameRevealed st
        mines = gameMines st
        triggeredMines = rev `Set.intersection` mines

isGameOver :: (UserAction, GameState) -> Bool
isGameOver (act, st) = isWin st || isLoss act st


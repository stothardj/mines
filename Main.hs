import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle
import Text.Regex.Posix
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Maybe

data Dimensions = Dimensions { dimWidth :: Int, dimHeight :: Int}
data Location = Location { row :: Int, col :: Int}
              deriving (Eq,Ord,Show)
data GameState = GameState { gameMines :: Set Location,
                             gameFlags :: Set Location,
                             gameRevealed :: Set Location,
                             gameDimensions :: Dimensions }
data Difficulty = Easy | Medium | Hard
data UserAction = RevealLocation | FlagLocation
                deriving Show

mineLocations :: MonadRandom m => Dimensions -> Int -> m (Set Location)
mineLocations dim numMines = do
  mines <- shuffleM $ Location <$> [1..dimWidth dim] <*> [1..dimHeight dim]
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

showLoc :: GameState -> Location -> Char
showLoc state loc
  | loc `Set.member` flags = 'F'
  | loc `Set.notMember` revealed = '.'
  | loc `Set.member` mines = '*'
  | otherwise = intToDigit $ Map.findWithDefault 0 loc (numNeighbors mines)
  where mines = gameMines state
        flags = gameFlags state
        revealed = gameRevealed state

showRow :: GameState -> Int -> Int -> String
showRow state width r = map (showLoc state . Location r) [1..width]

showBoard :: GameState -> String
showBoard state = unlines $ map (showRow state width) [1..height]
  where dim = gameDimensions state
        width = dimWidth dim
        height = dimHeight dim

floodfill :: (Location -> Bool) -> Set Location -> Location -> Set Location
floodfill isBlocking revealed loc
  | loc `Set.member` revealed = revealed
  | isBlocking loc = loc `Set.insert` revealed
  | otherwise = foldl f (loc `Set.insert` revealed) (neighbors loc)
  where f = floodfill isBlocking

isOnBoard :: Dimensions -> Location -> Bool
isOnBoard dim loc = r >= 1 && c >= 1 && r <= height && c <= width
  where r = row loc
        c = col loc
        height = dimHeight dim
        width = dimWidth dim

hasNeighbors :: Set Location  -> Location -> Bool
hasNeighbors mines loc = loc `Map.member` numNeighbors mines

reveal' :: GameState -> Location -> Set Location
reveal' state loc
  | loc `Set.member` mines = loc `Set.insert` revealed
  | loc `Set.member` flags = revealed
  | loc `Set.member` revealed = revealed -- TODO: reveal neighbors you could be trivially certain of
  | otherwise = floodfill (\l -> (hasNeighbors mines l) || (not (isOnBoard dim l))) revealed loc
  where dim = gameDimensions state
        mines = gameMines state
        flags = gameFlags state
        revealed = gameRevealed state

reveal :: GameState -> Location -> GameState
reveal state loc = state { gameRevealed = reveal' state loc }

flag' :: GameState -> Location -> Set Location
flag' state loc
  | loc `Set.member` flags = loc `Set.delete` flags
  | isOnBoard dim loc = loc `Set.insert` flags
  | otherwise = flags
  where dim = gameDimensions state
        flags = gameFlags state

flag :: GameState -> Location -> GameState
flag state loc = state { gameFlags = flag' state loc}

tryParseLocation :: String -> Maybe Location
tryParseLocation s
  | null matches = Nothing
  | otherwise = Just $ Location r c
  where
    matches :: [[String]]
    matches = s =~ "([0-9]+),([0-9]+)"
    r = read $ matches !! 0 !! 1
    c = read $ matches !! 0 !! 2

tryQuery :: String -> (String -> Maybe b) -> MaybeT IO b
tryQuery msg parser = do
  lift $ putStrLn msg
  MaybeT $ liftM parser getLine

query :: String -> (String -> Maybe r) -> IO r
query msg = liftM fromJust . runMaybeT . msum . repeat . tryQuery msg

queryLocation :: IO Location
queryLocation = query "Enter location as row,col (1,1 is the top-left)" tryParseLocation

queryAction :: IO UserAction
queryAction = query "Enter Action: [f]lag, [r]eveal" (\s -> case s of "f" -> Just FlagLocation
                                                                      "r" -> Just RevealLocation
                                                                      _ -> Nothing)

queryDifficulty :: IO Difficulty
queryDifficulty = query "Enter Difficulty: [e]asy, [m]edium, [h]ard" (\s -> case s of
                                                                           "e" -> Just Easy
                                                                           "m" -> Just Medium
                                                                           "h" -> Just Hard
                                                                           _ -> Nothing)

gameStart :: Difficulty -> IO GameState
gameStart diff = do
  g <- getStdGen
  mines <- evalRandT (mineLocationsForDifficulty diff) g
  return GameState {gameMines = mines,
                    gameFlags = Set.empty,
                    gameRevealed = Set.empty,
                    gameDimensions = dimensionsForDifficulty diff}

isGameOver :: GameState -> Bool
isGameOver state = (not . Set.null $ triggeredMines) -- Lose
                   || (Set.null $ allLocs `Set.difference` correctFlags `Set.difference` rev) -- Win
  where rev = gameRevealed state
        mines = gameMines state
        flags = gameFlags state
        dim = gameDimensions state
        triggeredMines = rev `Set.intersection` mines
        correctFlags = flags `Set.intersection` mines
        allLocs = Set.fromList $ Location <$> [1..dimHeight dim] <*> [1..dimWidth dim]

gameIteration' :: GameState -> IO GameState
gameIteration' state = do
  putStrLn $ showBoard state
  action <- queryAction
  loc <- queryLocation
  return $ case action of
            RevealLocation -> reveal state loc
            FlagLocation -> flag state loc

gameIteration :: GameState -> MaybeT IO GameState
gameIteration state = do
  newState <- lift $ gameIteration' state
  guard . not $ isGameOver newState
  return newState

iter_ :: Monad m => (a -> MaybeT m a) -> a -> m ()
iter_ f s = do
  ns <- runMaybeT (f s)
  case ns of
   Just n -> iter_ f n
   Nothing -> return ()

playGame :: IO ()
playGame = do
  diff <- queryDifficulty
  startState <- gameStart diff
  iter_ gameIteration startState

main :: IO ()
main = do
  putStrLn "Welcome to mines!"
  playGame
  putStrLn "Game over"


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
import Control.Monad.Trans.State.Lazy
import Control.Monad.Loops

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

showLoc :: GameState -> Location -> Char
showLoc st loc
  | loc `Set.member` flags = 'F'
  | loc `Set.notMember` revealed = '.'
  | loc `Set.member` mines = '*'
  | otherwise = intToDigit $ Map.findWithDefault 0 loc (numNeighbors mines)
  where mines = gameMines st
        flags = gameFlags st
        revealed = gameRevealed st

showRow :: GameState -> Int -> Int -> String
showRow st nCols r = map (showLoc st . Location r) [1..nCols]

showBoard :: GameState -> String
showBoard st = unlines $ map (showRow st nCols) [1..nRows]
  where dim = gameDimensions st
        nRows = numRows dim
        nCols = numCols dim

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

tryParseLocation :: String -> Maybe Location
tryParseLocation s
  | null matches = Nothing
  | otherwise = Just $ Location r c
  where
    matches :: [[String]]
    matches = s =~ "([0-9]+),([0-9]+)"
    r = read $ matches !! 0 !! 1
    c = read $ matches !! 0 !! 2

tryGetLocation :: Dimensions -> String -> Either String Location
tryGetLocation dim s = case tryParseLocation s of
                    Nothing -> Left "Could not parse location"
                    Just l -> if isOnBoard dim l then Right l else Left "Location is off the board"

tryQuery :: String -> (String -> Maybe b) -> MaybeT IO b
tryQuery msg parser = do
  lift $ putStrLn msg
  MaybeT $ liftM parser getLine

tryQueryWithResponse :: String -> (String -> Either String b) -> IO (Either String b)
tryQueryWithResponse msg parser = do
  putStrLn msg
  liftM parser getLine

query :: String -> (String -> Maybe r) -> IO r
query msg = liftM fromJust . runMaybeT . msum . repeat . tryQuery msg

queryWithResponse :: String -> (String -> Either String b) -> IO b
queryWithResponse msg parser = do
  resp <- tryQueryWithResponse msg parser
  case resp of
   Left s -> putStrLn s >> queryWithResponse msg parser
   Right l -> return l

queryLocation :: Dimensions -> IO Location
queryLocation dim = queryWithResponse "Enter location as row,col (1,1 is the top-left)" (tryGetLocation dim)

queryAction :: IO UserAction
queryAction = query "Enter Action: [f]lag, [r]eveal, [q]uit" (\s -> case s of "f" -> Just FlagLocation
                                                                              "r" -> Just RevealLocation
                                                                              "q" -> Just Quit
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

execAction :: UserAction -> GameState -> IO GameState
execAction RevealLocation st = do
  loc <- queryLocation (gameDimensions st)
  return $ reveal st loc
execAction FlagLocation st = do
  loc <- queryLocation (gameDimensions st)
  return $ flag st loc
execAction Quit st = return st

gameIteration' :: GameState -> IO (UserAction, GameState)
gameIteration' st = do
  putStrLn $ showBoard st
  action <- queryAction
  nst <- execAction action st
  return (action, nst)

gameIteration :: StateT GameState IO (UserAction, GameState)
gameIteration = do
  cstate <- get
  (action, nstate) <- lift $ gameIteration' cstate
  put nstate
  return (action, nstate)

revealLoss :: GameState -> IO ()
revealLoss st = do
  putStrLn "Boom!"
  putStrLn $ showBoard (st {gameRevealed = Set.fromList $ allLocations (gameDimensions st),
                            gameFlags = Set.empty})

playGame :: IO ()
playGame = do
  diff <- queryDifficulty
  startState <- gameStart diff
  (finalAction, finalState) <- evalStateT (iterateUntil isGameOver gameIteration) startState
  putStrLn $ showBoard finalState
  when (isLoss finalAction finalState) $ revealLoss finalState

main :: IO ()
main = do
  putStrLn "Welcome to mines!"
  playGame
  putStrLn "Game over"


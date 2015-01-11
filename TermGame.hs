module TermGame(playGame) where

import Mines
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Text.Regex.Posix
import Data.Char
import qualified Data.Map.Strict as Map

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

execAction :: UserAction -> GameState -> IO GameState
execAction RevealLocation st = do
  loc <- queryLocation (gameDimensions st)
  return $ reveal st loc
execAction FlagLocation st = do
  loc <- queryLocation (gameDimensions st)
  return $ flag st loc
execAction Quit st = return st

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

revealLoss :: GameState -> IO ()
revealLoss st = do
  putStrLn "Boom!"
  putStrLn $ showBoard (st {gameRevealed = Set.fromList $ allLocations (gameDimensions st),
                            gameFlags = Set.empty})

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

playGame :: IO ()
playGame = do
  diff <- queryDifficulty
  startState <- gameStart diff
  (finalAction, finalState) <- evalStateT (iterateUntil isGameOver gameIteration) startState
  putStrLn $ showBoard finalState
  when (isLoss finalAction finalState) $ revealLoss finalState

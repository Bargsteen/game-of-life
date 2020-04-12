module Main where

import Prelude

import Data.Array as Arr
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console as C
import Matrix as M

main :: Effect Unit
main = Aff.launchAff_ $ runNGames 300 pentadecathlon

runNGames :: Int -> Game -> Aff.Aff Unit
runNGames n game = do
  liftEffect $ C.clear
  liftEffect $ C.log $ showGame game
  Aff.delay (Aff.Milliseconds 500.0)
  if n > 0
  then runNGames (n - 1) (step game)
  else liftEffect $ C.log ("Done")

-- Data

type Game = M.Matrix Cell

data Cell = Alive | Dead

instance showCell :: Show Cell where
  show Alive = "ɷ"
  show Dead  = "."

derive instance eqCell :: Eq Cell

-- Primary functions

step :: Game -> Game
step game = M.indexedMap (stepCell game) game

stepCell :: Game -> Int -> Int -> Cell -> Cell
stepCell game x y currentState = case currentState, aliveNeighbourCount of
  Alive, 2 -> Alive
  Alive, 3 -> Alive
  Dead , 3 -> Alive
  _, _     -> Dead

  where
    neighbourPositions = [ Tuple (x-1) (y-1), Tuple (x) (y-1), Tuple (x+1) (y-1)
                         , Tuple (x-1) (y)  ,                  Tuple (x+1) (y)
                         , Tuple (x-1) (y+1), Tuple (x) (y+1), Tuple (x+1) (y+1)
                         ]
    getNeighbour (Tuple row col) = M.get row col game
    neighbours = Arr.catMaybes <<< map getNeighbour $ neighbourPositions
    aliveNeighbourCount = Arr.length <<< Arr.filter ((==) Alive) $ neighbours


-- Helpers

showGame :: Game -> String
showGame game = case mRows' of
  Nothing -> "Empty game.."
  Just rows -> Arr.intercalate "\n" rows
  where
    mRows :: Maybe (Array (Array String))
    mRows = (map <<< map) show <$> traverse (flip M.getRow game) (Arr.range 0 $ (M.height game) - 1)

    mRows' :: Maybe (Array String)
    mRows' = map (Arr.intercalate " ") <$> mRows


mkGame :: Array (Array Cell) -> Game
mkGame = fromMaybe M.empty <<< M.fromArray

mkRow :: Int -> Cell -> Array Cell
mkRow = Arr.replicate

mkDeadRow :: Int -> Array Cell
mkDeadRow len = mkRow len Dead

-- Games

-- Period = 2
blinker :: Game
blinker = mkGame [deadRow, deadRow, oscillator, deadRow, deadRow]
  where deadRow = mkDeadRow 5
        oscillator = [Dead, Alive, Alive, Alive, Dead]

-- Period = 15
pentadecathlon :: Game
pentadecathlon = mkGame [deadRow, deadRow, deadRow, twoRow, midRow, twoRow, deadRow, deadRow, deadRow]
  where deadRow = mkDeadRow 16
        twoRow = mkDeadRow 5 <> [Alive] <> mkDeadRow 4 <> [Alive] <> mkDeadRow 5
        midRow = Arr.concat $ uncurry mkRow <$> [Tuple 3 Dead, Tuple 2 Alive, Tuple 1 Dead, Tuple 4 Alive, Tuple 1 Dead, Tuple 2 Alive, Tuple 3 Dead]

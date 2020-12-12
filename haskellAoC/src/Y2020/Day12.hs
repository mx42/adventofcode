module Y2020.Day12 (y20day12) where

data Instruction =
  GoNorth Int |
  GoSouth Int |
  GoWest Int |
  GoEast Int |
  TurnLeft |
  TurnRight |
  TurnAround |
  Forward Int deriving (Show)

-- Orientations
-- Right = 0
-- Bottom = 1
-- Left = 2
-- Top = 3
data State = State {
  pos :: (Int, Int)
  , turn :: Int
  } deriving (Show)

data StateP2 = StateP2 {
  shipPos :: (Int, Int)
  , waypointPos :: (Int, Int)
  } deriving (Show)

parseInput :: String -> Instruction
parseInput ('N':n) = GoNorth (read n)
parseInput ('S':n) = GoSouth (read n)
parseInput ('W':n) = GoWest (read n)
parseInput ('E':n) = GoEast (read n)
parseInput ('F':n) = Forward (read n)
parseInput "L90" = TurnLeft
parseInput "L270" = TurnRight
parseInput "R90" = TurnRight
parseInput "R270" = TurnLeft
parseInput (_:"180") = TurnAround
parseInput _ = error "Wrong instruction"

applyInstruction :: State -> Instruction -> State
applyInstruction (State (x, y) t) (GoNorth n) = State (x, y + n) t
applyInstruction (State (x, y) t) (GoSouth n) = State (x, y -n) t
applyInstruction (State (x, y) t) (GoWest n) = State (x - n, y) t
applyInstruction (State (x, y) t) (GoEast n) = State (x + n, y) t
applyInstruction (State p t) TurnRight = State p $ (t + 1) `mod` 4
applyInstruction (State p t) TurnAround = State p $ (t + 2) `mod` 4
applyInstruction (State p t) TurnLeft = State p $ (t + 3) `mod` 4
applyInstruction (State (x, y) t) (Forward n) = State (moveForward t) t
  where moveForward 0 = (x + n, y)
        moveForward 1 = (x, y - n)
        moveForward 2 = (x - n, y)
        moveForward 3 = (x, y + n)

applyInstructionP2 :: StateP2 -> Instruction -> StateP2
applyInstructionP2 (StateP2 sp (x, y)) (GoNorth n) = StateP2 sp (x, y + n)
applyInstructionP2 (StateP2 sp (x, y)) (GoSouth n) = StateP2 sp (x, y - n)
applyInstructionP2 (StateP2 sp (x, y)) (GoWest n) = StateP2 sp (x - n, y)
applyInstructionP2 (StateP2 sp (x, y)) (GoEast n) = StateP2 sp (x + n, y)
applyInstructionP2 (StateP2 sp (x, y)) TurnLeft = StateP2 sp (-y, x)
applyInstructionP2 (StateP2 sp (x, y)) TurnRight = StateP2 sp (y, -x)
applyInstructionP2 (StateP2 sp (x, y)) TurnAround = StateP2 sp (-x, -y)
applyInstructionP2 (StateP2 (x, y) wp@(x', y')) (Forward n) = StateP2 (x + (n * x'), y + (n * y')) wp

y20day12 :: [String] -> (String, String)
y20day12 input = (part1, part2)
  where part1 = show $ sum $ map abs $ [fst endposP1, snd endposP1]
        part2 = show $ sum $ map abs $ [fst endposP2, snd endposP2]
        start = State (0, 0) 0
        startP2 = StateP2 (0, 0) (10, 1)
        journey = map parseInput input
        (State endposP1 _) = foldl applyInstruction start journey
        (StateP2 endposP2 _) = foldl applyInstructionP2 startP2 journey

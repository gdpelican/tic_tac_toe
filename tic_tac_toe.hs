import Data.Char

data Square = Square Char
type BoardRow = (Square, Square, Square)
type Board = (BoardRow, BoardRow, BoardRow, Bool)

main = promptMove emptyBoard
       where empty = Square ' '
             emptyRow = (empty, empty, empty)
             emptyBoard = (emptyRow, emptyRow, emptyRow, True)

promptMove :: Board -> IO()
promptMove board = do
  printBoard board
  if (canPlay board)
    then do play <- getLine
            promptMove (makeMove board (toCoords play))
    else do printResult board

canPlay :: Board -> Bool
canPlay board = True

makeMove :: Board -> (Int, Int) -> Board
makeMove (a, b, c, move) (0, col) = ((makeColMove ([a,b,c] !! 0) col move), b, c, (not move))
makeMove (a, b, c, move) (1, col) = (a, (makeColMove ([a,b,c] !! 1) col move), c, (not move))
makeMove (a, b, c, move) (2, col) = (a, b, (makeColMove ([a,b,c] !! 2) col move), (not move))

makeColMove :: BoardRow -> Int -> Bool -> BoardRow
makeColMove (a, b, c) 0 move = ((moveToSquare move), b, c)
makeColMove (a, b, c) 1 move = (a, (moveToSquare move), c)
makeColMove (a, b, c) 2 move = (a, b, (moveToSquare move))

moveToSquare :: Bool -> Square
moveToSquare m = Square (moveToChar m)

printBoard :: Board -> IO ()
printBoard (a,b,c,m) = do
  putRow a
  putStrLn "-------"
  putRow b
  putStrLn "-------"
  putRow c
  putStrLn [(moveToChar m)]

printResult :: Board -> IO ()
printResult board = putStrLn "Game finished!"

putRow :: BoardRow -> IO ()
putRow (a, b, c) = putStrLn (" " ++ (squareToStr a) ++ "|" ++ (squareToStr b) ++ "|" ++ (squareToStr c) ++ " ")
  where squareToStr (Square c) = [c]

moveToChar :: Bool -> Char
moveToChar s = case s of
  True -> 'X'
  False -> 'O'

toCoords :: String -> (Int, Int)
toCoords s = (digitToInt (head s), digitToInt (last s))

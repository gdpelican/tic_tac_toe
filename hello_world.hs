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
  if False
    then return ()
    else do
      printBoard board
      play <- getLine
      promptMove (makeMove board (toCoords play))

makeMove :: Board -> (Int, Int) -> Board
makeMove (rowA, rowB, rowC, move) (0,col) = ((makeRowMove rowA col move), rowB, rowC, (not move))
makeMove (rowA, rowB, rowC, move) (1,col) = (rowA, (makeRowMove rowB col move), rowC, (not move))
makeMove (rowA, rowB, rowC, move) (2,col) = (rowA, rowB, (makeRowMove rowC col move), (not move))

makeRowMove :: BoardRow -> Int -> Bool -> BoardRow
makeRowMove (_,b,c) 0 move = ((Square (moveToChar move)), b, c)
makeRowMove (a,_,c) 1 move = (a, (Square (moveToChar move)), c)
makeRowMove (a,b,_) 2 move = (a, b, (Square (moveToChar move)))

printBoard :: Board -> IO ()
printBoard (a,b,c,m) = do
  putRow a
  putStrLn "-------"
  putRow b
  putStrLn "-------"
  putRow c
  putStrLn [(moveToChar m)]

putRow :: BoardRow -> IO ()
putRow (a, b, c) = putStrLn (" " ++ (squareToStr a) ++ "|" ++ (squareToStr b) ++ "|" ++ (squareToStr c) ++ " ")

moveToChar :: Bool -> Char
moveToChar s = case s of
  True -> 'X'
  False -> 'O'

squareToStr :: Square -> String
squareToStr (Square c) = [c]

toCoords :: String -> (Int, Int)
toCoords s = (digitToInt (head s), digitToInt (last s))

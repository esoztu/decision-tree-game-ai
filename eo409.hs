import Base
import Control.Concurrent
import DecisionTree

-- Question 1.1
showBoard :: Board -> String
showBoard b = unlines (map showRow (splitRows b))

showCell :: Maybe Player -> String
showCell (Just One) = "X"
showCell (Just Two) = "O"
showCell Nothing    = "_"

splitRows :: Board -> [[Maybe Player]]
splitRows [] = []
splitRows xs = take 3 xs : splitRows (drop 3 xs)

showRow :: [Maybe Player] -> String
showRow cells = unwords (map showCell cells)

-- Question 1.2

toPos :: (Int, Int) -> Maybe Int
toPos (row, col)
  | row >= 0 && row <= 2 && col >= 0 && col <= 2 = Just (row * 3 + col)
  | otherwise = Nothing

-- Question 1.3

lookupBoard :: Board -> Int -> Maybe Player
lookupBoard board pos =
  if pos >= 0 && pos < length board
    then board !! pos
    else Nothing

-- Question 1.4

addToGameBoard :: Board -> Int -> Player -> Maybe Board
addToGameBoard board pos player =
  case splitAt pos board of
    (before, x:after) ->
      case x of
        Nothing -> Just (before ++ (Just player : after))
        Just _  -> Nothing
    _ -> Nothing  -- Position out of bounds


-- Question 1.5

checkWin :: Board -> Maybe Player
checkWin board = checkLines winningLines
  where
    -- List of winning triples (indices)
    winningLines :: [[Int]]
    winningLines =
      [ [0, 1, 2]  -- top row
      , [3, 4, 5]  -- middle row
      , [6, 7, 8]  -- bottom row
      , [0, 3, 6]  -- left column
      , [1, 4, 7]  -- middle column
      , [2, 5, 8]  -- right column
      , [0, 4, 8]  -- main diagonal
      , [2, 4, 6]  -- anti-diagonal
      ]

    -- Look through each line for a winner
    checkLines [] = Nothing
    checkLines (line:rest) =
      case map (board !!) line of
        [Just p1, Just p2, Just p3]
          | p1 == p2 && p2 == p3 -> Just p1
        _ -> checkLines rest

-- Question 1.6
-- Print a horizontal line of 20 hyphens

hline :: IO ()
hline = putStrLn (replicate 20 '-')

-- Question 2.7

select :: Player -> Chan Int -> Chan Int -> Chan Int
select One chan1 _    = chan1
select Two _ chan2    = chan2

-- Question 2.8

writeChanTwice :: Chan a -> a -> IO ()
writeChanTwice chan val = do
  writeChan chan val
  writeChan chan val

-- Question 2.9

gameServer :: Player -> Board
           -> Chan Int -> Chan Int -> Chan Result
           -> IO (Maybe Player)
gameServer player board chan1 chan2 resultChan = do

  -- (b) Prompt the current player for a move
  putStrLn $ "Player " ++ show player ++ ", enter your row (1-3) and then column (A-C):"

  -- (c) Receive a move from the current player's channel
  let moveChan = select player chan1 chan2
  move <- readChan moveChan

  -- (d) Print a message describing the attempted move
  putStrLn $ "Player " ++ show player ++ " attempted to move to " ++ show move

  -- (e) Try to add the move to the board
  case addToGameBoard board move player of

    -- (e.i) If the move is invalid (position already taken or out of bounds)
    Nothing -> do
      putStrLn "Invalid move! That space is already taken."
      -- Notify both players that the game continues with the current board
      writeChanTwice resultChan (Continue board)
      -- Flip the player and continue the game
      gameServer (flipPlayer player) board chan1 chan2 resultChan

    -- (e.ii) If the move is valid
    Just newBoard -> do
      -- A. Print the updated board
      putStr $ showBoard newBoard
      hline

      -- B. Check for a winner
      case checkWin newBoard of

        -- (e.ii.A) If a player has won
        Just winner -> do
          writeChanTwice resultChan (Win winner newBoard)
          -- Return the winner
          putStrLn $ "Player " ++ show player ++ " wins!"
          return (Just winner)

        -- (e.ii.B or C) No winner yet
        Nothing ->
          if any (== Nothing) newBoard then do
            writeChanTwice resultChan (Continue newBoard)
            gameServer (flipPlayer player) newBoard chan1 chan2 resultChan
          else do
            writeChanTwice resultChan (Draw newBoard)
            -- Return Nothing to signal a draw
            return Nothing

-- Question 2.10

gameServerStart :: Player
                -> (Chan Coordination, Chan Coordination)
                -> (Chan Int, Chan Int)
                -> (Int, Int)
                -> Chan Result
                -> IO ()
gameServerStart startingPlayer (coord1, coord2) (move1, move2) (score1, score2) resultChan = do
  hline
  let emptyBoard = replicate 9 Nothing

  writeChanTwice resultChan (Continue emptyBoard)

  result <- gameServer startingPlayer emptyBoard move1 move2 resultChan

  let (newScore1, newScore2) = case result of
        Just One -> (score1 + 1, score2)
        Just Two -> (score1, score2 + 1)
        Nothing  -> (score1, score2)

  hline
  putStrLn $ "Score Board: Player One = " ++ show newScore1 ++ " | Player Two = " ++ show newScore2
  hline
  putStrLn "Play again? Player One needs to say Y/N?"

  response <- readChan coord1

  case response of
    Stop -> do
      writeChan coord2 Stop
      putStrLn "End of tournament!"

    Again -> do
      writeChan coord2 Again
      gameServerStart (flipPlayer startingPlayer)
                      (coord1, coord2)
                      (move1, move2)
                      (newScore1, newScore2)
                      resultChan


-- Question 2.11
startGame :: (Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ())
          -> (Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ())
          -> IO ()
startGame player1Func player2Func = do
  coordChan1 <- newChan
  coordChan2 <- newChan
  moveChan1  <- newChan
  moveChan2  <- newChan
  resultChan <- newChan

  _ <- forkIO $ player1Func One coordChan1 moveChan1 resultChan
  _ <- forkIO $ player2Func Two coordChan2 moveChan2 resultChan

  gameServerStart One (coordChan1, coordChan2) (moveChan1, moveChan2) (0, 0) resultChan


-- Question 3.12
parseInput :: String -> String -> Maybe Int
parseInput rowStr colStr = do
  -- Convert row string ("1", "2", "3") to 0-based row index
  row <- case rowStr of
    "1" -> Just 0
    "2" -> Just 1
    "3" -> Just 2
    _   -> Nothing

  -- Convert column string ("A", "B", "C", case-insensitive) to 0-based col index
  col <- case colStr of
    "A" -> Just 0
    "a" -> Just 0
    "B" -> Just 1
    "b" -> Just 1
    "C" -> Just 2
    "c" -> Just 2
    _   -> Nothing

  -- Use toPos to turn (row, col) into board index (0–8)
  toPos (row, col)



-- Question 3.13

humanPlayer :: Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()
humanPlayer player coordChan moveChan resultChan = 
    humanPlayerTournament player coordChan moveChan resultChan

-- Handle the tournament coordination and loop over games
humanPlayerTournament :: Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()
humanPlayerTournament player coordChan moveChan resultChan = do
  -- Play a single game
  humanPlayerGame player moveChan resultChan

  -- At the end of a game, handle coordination
  case player of
    One -> do
      answer <- getLine
      let msg = if answer `elem` ["Y", "y"] then Again else Stop
      writeChan coordChan msg
      if msg == Again
        then humanPlayerTournament player coordChan moveChan resultChan
        else do
            return ()


    Two -> do
      -- Player Two waits for Player One's coordination decision
      msg <- readChan coordChan
      if msg == Again
        then humanPlayerTournament player coordChan moveChan resultChan
        else 
            return ()


humanPlayerGame :: Player -> Chan Int -> Chan Result -> IO ()
humanPlayerGame player moveChan resultChan = do
  -- Ask for move input from the user
  rowStr <- getLine
  colStr <- getLine

  case parseInput rowStr colStr of
    Nothing -> do
      humanPlayerGame player moveChan resultChan

    Just move -> do
      -- Send the move to the server
      writeChan moveChan move

      -- Wait for result after our move
      result <- readChan resultChan
      case result of
        Win p _ -> do
          return ()
        Draw _ -> do
          return ()
        Continue _ -> do
          -- Opponent's turn — wait for the second result
          result2 <- readChan resultChan
          case result2 of
            Win p _ -> do
              return ()
            Draw _ -> do
              return ()
            Continue _ -> do
              -- Game still going — recurse and continue playing
              humanPlayerGame player moveChan resultChan

-- Question 4.14
aiPlayer :: Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()
aiPlayer player coordChan moveChan resultChan =
  loop (header, []) (learnTree (header, []) bestGain)
  where
    loop trainingSet tree = do
      Continue board <- readChan resultChan
      (result, finalBoard) <- aiGame board tree
      let label = case result of
                    Just p | p == player -> Yes
                    _                    -> No
          updatedData = addRow trainingSet (boardToRow finalBoard, label)
          updatedTree = learnTree updatedData bestGain
      msg <- readChan coordChan
      case msg of
        Stop  -> return ()
        Again -> loop updatedData updatedTree

    aiGame board tree = gameLoop board
      where
        gameLoop b =
          if currentPlayer b == player
            then do
              move <- chooseBestMove player b tree
              writeChan moveChan move
              msg <- readChan resultChan
              case msg of
                Win p b'    -> return (Just p, b')
                Draw b'     -> return (Nothing, b')
                Continue b' -> gameLoop b'
            else do
              msg <- readChan resultChan
              case msg of
                Win p b'    -> return (Just p, b')
                Draw b'     -> return (Nothing, b')
                Continue b' -> gameLoop b'

chooseBestMove :: Player -> Board -> DecisionTree -> IO Int
chooseBestMove player board tree = return (choose moves rows (-1) (-1.0))
  where
    moves = availableMoves board
    rows = map (\pos -> boardToRow (applyMove board pos player)) moves
    choose [] [] bestIdx _ = bestIdx
    choose (m:ms) (r:rs) bestIdx bestScore =
      let score = confidence tree player r in
      if score > bestScore
        then choose ms rs m score
        else choose ms rs bestIdx bestScore

confidence :: DecisionTree -> Player -> Row -> Float
confidence tree p row =
  case infer tree header row of
    Just (Yes, prob) -> prob
    _                -> 0.0

currentPlayer :: Board -> Player
currentPlayer board =
  let n = length (filter (/= Nothing) board)
  in if even n then One else Two

boardToRow :: Board -> Row
boardToRow [] = []
boardToRow (x:xs) = show x : boardToRow xs

availableMoves :: Board -> [Int]
availableMoves board = availableHelper board 0

availableHelper :: Board -> Int -> [Int]
availableHelper [] _ = []
availableHelper (Nothing:xs) i = i : availableHelper xs (i + 1)
availableHelper (_:xs) i       = availableHelper xs (i + 1)

applyMove :: Board -> Int -> Player -> Board
applyMove board pos player =
  case addToGameBoard board pos player of
    Just b  -> b
    Nothing -> board

header :: Header
header = makeHeader 0 8

makeHeader :: Int -> Int -> [String]
makeHeader i end =
  if i > end then [] else show i : makeHeader (i + 1) end

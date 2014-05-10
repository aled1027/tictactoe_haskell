-- finish the fuck out of this
-- http://www.haskell.org/haskellwiki/Haskell_Quiz/TicTacToe/Solution_Abhinav
-- use more anonymous functions?
-- to do:
--  1. add ai minimax -- should be pretty easy functionally. Consider using list functor to gain nondeterminism?
--      - might be overkill. May just need simply list manipulation and recursion
--      - use maybe monad and state monad, if needed -- probably need both. Probalby make it much cleaner
--      - think about in what spots you would need side effects and in what spots things can remain pure
--

import Control.Monad
import qualified Data.List as DL
import qualified Data.List.Split as S
import Aid
import Boring

isGameOver :: Board -> Bool
isGameOver board = if b1 || b2 || b3 then True else False
    where subseq = S.splitEvery 3 board
          rows = map isRowSame subseq
          cols = map isRowSame (DL.transpose subseq)
          diags = map isRowSame [map (board!!) [0,4,5],map (board!!) [2,4,6]]
          b1 = True `elem` rows
          b2 = True `elem` cols
          b3 = True `elem` diags

isRowSame :: String -> Bool
isRowSame row = if b && n == 1 then True else False
    where b = not $ ' ' `elem` row
          n = length (DL.nub row)  
          -- maybe faster to use foldl instead of nub
          -- look here if we need to optimize. This will be called a lot

-- need to add in maybe monad. -- Bad Input ---> Nothing
playerTurn :: Board -> Char -> IO Board
playerTurn board letter = do
    printBoard board
    putStrLn (letter:"s turn. Enter your move.")
    pos <- getLine
    -- add -- check to see if move is valid
    let board' = insert letter (read pos) board
    return board'

computerTurn :: Board -> Char -> IO Board
computerTurn board letter =  playerTurn board letter

gameLoop :: Board -> Char -> Char -> IO ()
gameLoop board playerLetter computerLetter = do
    putStrLn "----------------------------"
    board' <- playerTurn board playerLetter 
    if isGameOver board' 
        then do putStrLn "***Player wins***"
                return ()
        else do board'' <- computerTurn board' computerLetter
            if isGameOver board''
                then do putStrLn "***Computer wins***"
                        return ()
                else do gameLoop board'' playerLetter computerLetter

main'' = do 
    prologue 
    let playerLetter = 'X'
    let computerLetter = 'O'
    gameLoop emptyBoard playerLetter computerLetter
    putStrLn "exiting"

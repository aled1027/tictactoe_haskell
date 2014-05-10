module Boring where

import qualified Data.List.Split as S

type Board = String

emptyBoard = [' ' | x <- [1..9]]

-- can use intersperse or intercalate check Data.Char
printBoard board = do
    let (a:b:c:rest) = S.splitEvery 3 board
    let a' = concat [x:"|" | x <- init a] ++ [(last a)]
    let b' = concat [x:"|" | x <- init b] ++ [(last b)]
    let c' = concat [x:"|" | x <- init c] ++ [(last c)]
    putStrLn a'
    putStrLn b'
    putStrLn c' 

prologue :: IO ()
prologue = do
    putStrLn ""
    putStrLn "Before move, the board will print and you will be prompted to enter a move"
    putStrLn "To enter a move, type in the index of the cell where you wish to move."
    putStrLn "The indices of the board are formatted as follows:"
    printBoard ['1'..'9']
    putStrLn "Good Luck!"
    putStrLn ""




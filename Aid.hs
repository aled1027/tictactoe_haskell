module Aid where

insert :: Char -> Int -> String -> String
insert val 1 (x:xs) = val:xs
insert val pos (x:xs) = x:(insert val (pos-1) xs)
----------Functions to aid computation-----
--
-- does not use c array style indices
-- general use function



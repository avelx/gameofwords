module Lib
    (  formatGrid
     , outputGrid
     , skew
     , findWord
     , findWords  
     , findWordInLine
     , someString
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)
type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn ( formatGrid grid)

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map ident ls)
  where ident line = '_' : line

formatGrid :: Grid -> String
formatGrid = unlines

getLines :: Grid -> [String]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse vertical)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  in lines  ++ (map reverse lines)

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

findWord :: Grid -> String -> Maybe String
findWord grid word = 
   let lines = getLines grid
       found = or $ map ( findWordInLine word ) lines
   in if found then Just word else Nothing

findWords grid words = 
   let foundWords = map (findWord grid) words
   in catMaybes foundWords 

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

someString = "someString"
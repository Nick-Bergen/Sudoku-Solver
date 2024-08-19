--Define Sudoku
type Matrix = [[Int]]
type Pos = (Int, Int)

sudoku :: Matrix
sudoku =
    [
        [0,8,0, 4,0,7,  0,0,0],
        [7,3,0, 1,2,0,  0,9,0],
        [2,0,9, 0,8,0,  7,6,0],

        [0,0,0, 0,3,1,  0,0,0],
        [0,0,2, 0,7,8,  9,1,0],
        [1,9,0, 0,0,5,  0,8,3],

        [8,0,1, 0,6,0,  0,0,0],
        [0,2,0, 3,0,4,  8,7,1],
        [4,0,3, 0,0,9,  0,5,0]
    ]

--Readability functions
sudokuSize :: Matrix -> Int
sudokuSize = length

boxSize :: Int
boxSize = 3

--Getters
getRow :: Matrix -> Int -> [Int]
getRow matrix row = matrix !! row --Get list of Int from given row from given sudoku

getColumn :: Matrix -> Int -> [Int]
getColumn matrix column = map (!! column) matrix --Takes the column's number and fetches it from each row from the sudoku

getBox :: Matrix -> Pos -> [Int]
getBox matrix (x, y) = [matrix !! (x' + xBoxCorner) !! (y' + yBoxCorner) | x' <- [0..(boxSize - 1)], y' <- [0..(boxSize - 1)]] --Starting from the top left corner of the box take the relative numbers (0 to 3) of both dimensions to make a list of the 9 numbers in the box
    where
        (xBoxCorner, yBoxCorner) = (x - x `mod` boxSize, y - y `mod` boxSize) --Use modulo to determine the top left corner of the box based on the size of the sudoku boxes

getEmpty :: Matrix -> Maybe Pos
getEmpty matrix =
    case [(x, y) | x <- [0..(sudokuSize matrix - 1)], y <- [0..(sudokuSize matrix - 1)], matrix !! x !! y == 0] of --Limits possible positions from 0 to 9 (0 for empty). Then forms a list of all empty fields in the matrix
        [] -> Nothing --No 0 was found, this should mean the Sudoku has been solved
        (pos:_) -> Just pos --pos is the first item in the above formed list with _ being the rest of the list, since we only care about getting one we only return pos/the first one

--Validity checker
isValid :: Matrix -> Pos -> Int -> Bool
isValid matrix (x, y) numToCheck =
    notElem numToCheck (getRow matrix x)        &&  --numToCheck is NOT found in the given row
    notElem numToCheck (getColumn matrix y)     &&  --numToCheck is NOT found in the given column
    notElem numToCheck (getBox matrix (x, y))       --numToCheck is NOT found in the box based on the given position

--Update Matrix
updateMatrix :: Matrix -> Pos -> Int -> Matrix
updateMatrix matrix (x, y) numToInsert =
    take x matrix ++ --Makes a new temp list that exists out of row [0..r] based on the given matrix. ++ indicates that to this list the following should be added --Makes a new temp list that exists out of row [0..r] based on the given matrix. ++ indicates that to this list the following should be added
     --Makes a new temp list that exists out of row [0..r] based on the given matrix. ++ indicates that to this list the following should be added
    [take y (matrix !! x) ++ [numToInsert] ++ drop (y + 1) (matrix !! x )] ++ --Makes a temp list that exist out of the existing row at x but only up to the number that has to be inserted, this number replaces the one there, the remainder of the existing row at x gets added to this list with the drop function. This list forms the new row at the x coordinates --Makes a temp list that exist out of the existing row at x but only up to the number that has to be inserted, this number replaces the one there, the remainder of the existing row at x gets added to this list with the drop function. This list forms the new row at the x coordinates
    drop (x + 1) matrix --Makes a new temp list that exists out of row [r + 1..SudokuSize] based on the given matrix. This list gets added to the list set up with take

solveSudoku :: Matrix -> Maybe Matrix
solveSudoku matrix =
    case getEmpty matrix of
        Nothing -> Just matrix --There are no empty fields, Sudoku has been solved
        Just (x, y) -> tryNumber matrix (x, y) [1..(sudokuSize sudoku)] --In case of found 0 at cords (x, y) attempt to fill in the numbers 1 to sudokuSize

tryNumber :: Matrix -> Pos -> [Int] -> Maybe Matrix
tryNumber matrix pos [] = Nothing   --Returns nothing if called without a list of numbers. This includes after looping enough times to run out of numbers in the provided list
tryNumber matrix pos (first : rest) --Attempt to loop through provided numbers to see if any fit
    | isValid matrix pos first =
        case solveSudoku (updateMatrix matrix pos first) of --If valid update value at the given pos and then recusively call for the next empty field to be filled
            Nothing -> tryNumber matrix pos rest    --Even though valid, the sudoku could not be solved with this number in this pos, replace it by calling tryNumber again but without the current number in the list of numbers to attempt
            solution -> solution    --If this gets called that means that all empty fields have been filled with valid numbers. This means the sudoku has been solved.
    | otherwise = tryNumber matrix pos rest --The number isn't valid, attempt it again with the remaining numbers in the list

--Main function
main :: IO ()
main = do
  putStrLn "[START]"
  --print $ sudokuSize sudoku
  --print $ getRow sudoku 1
  --print $ getColumn sudoku 1
  --print $ getBox sudoku (5, 5)
  --print $ getEmpty sudoku
  --print $ isValid sudoku (0, 0) 6 --Confirmed with C# prototype to be True
  --print $ updateMatrix sudoku (0, 0) 6
  case solveSudoku sudoku of
    Nothing -> putStrLn "[ERROR] No solution can be found for this sudoku"
    Just solution -> print solution --Transform solution into a string and print it
  putStrLn "[FINISH]"

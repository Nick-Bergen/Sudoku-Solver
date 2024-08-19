using System.Data.Common;
using System.Diagnostics;

namespace Sudoku_Solver
{
    class Program
    {
        private int[,] Sudoku = {
            {0,8,0, 4,0,7,  0,0,0},
            {7,3,0, 1,2,0,  0,9,0},
            {2,0,9, 0,8,0,  7,6,0},

            {0,0,0, 0,3,1,  0,0,0},
            {0,0,2, 0,7,8,  9,1,0},
            {1,9,0, 0,0,5,  0,8,3},

            {8,0,1, 0,6,0,  0,0,0},
            {0,2,0, 3,0,4,  8,7,1},
            {4,0,3, 0,0,9,  0,5,0},
        };
        private int SudokuSize;

        static void Main(string[] args)
        {
            Console.WriteLine("[START]");
            Program p = new();
            p.Init();
            Console.WriteLine("[FINISH]");
        }

        private void Init()
        {
            Console.WriteLine("[TESTING] Init()");
            SudokuSize = Sudoku.GetLength(0);
            Console.WriteLine("[TESTING] SudokuSize: " + SudokuSize);
            if (SolveSudoku()) {
                PrintSudoku();
            }
            else
            {
                Console.WriteLine("[ERROR] The given sudoku could not be solved.");
            }
        }

        private bool SolveSudoku()
        {
            int[] next = getEmpty();
            if(next == null)
            {
                //Done
                Console.WriteLine("[TESTING] Sudoku should be done");
                return true;
            }

            //Loop through the numbers
            for (int num = 1; num <= SudokuSize; num++)
            {
                Sudoku[next[0], next[1]] = num;
                if (checkField(next))
                {
                    //Found fitting number, call SolveSudoku to try next one
                    if (SolveSudoku())
                    {
                        return true;
                    }
                }
                //Backtrack -> Gets called when current number doesn't fit or the next number returns false on their SolveSudoku
                Sudoku[next[0], next[1]] = 0;
            }
            return false;
        }

        private int[] getRow(int _row)
        {
            int[] returnInt = new int[9];
            for (int i = 0; i < SudokuSize; i++)
            {
                returnInt[i] = Sudoku[_row, i];
            }
            if (returnInt == null)
            {
                Console.WriteLine("[ERROR] Cannot find row!");
            }
            return returnInt;
        }

        private int[] getColumn(int _column)
        {
            int[] returnInt = new int[9];
            for (int i = 0; i < SudokuSize; i++)
            {
                returnInt[i] = Sudoku[i, _column];
            }
            if (returnInt == null)
            {
                Console.WriteLine("[ERROR] Cannot find column!");
            }
            return returnInt;
        }

        private int[] getBox(int[] _box)
        {
            int[] returnInt = new int[9];
            int index = 0;
            for (int i = 0; i < SudokuSize / 3; i++)
            {
                for (int j = 0; j < SudokuSize / 3; j++)
                {
                    returnInt[index] = Sudoku[i + _box[0], j + _box[1]];
                    index++;
                }
            }
            if (returnInt == null)
            {
                Console.WriteLine("[ERROR] Cannot find box!");
            }
            return returnInt;
        }

        private int[] getEmpty()
        {
            for (int i = 0; i < SudokuSize; i++)
            {
                for (int j = 0; j < SudokuSize; j++)
                {
                    if (Sudoku[i, j] == 0)
                    {
                        int[] returnint = { i, j};
                        Console.WriteLine("[TESTING] found empty at [" + i + ", " + j + "]");
                        return returnint;
                    }
                }
            }
            //Sudoku should be solved
            return null;
        }

        private Boolean checkField(int[] field)
        {
            int value = Sudoku[field[0], field[1]];
            //Is valid number
            if (value <= 0 || value > SudokuSize)
            {
                //Console.WriteLine("[TESTING] Invalid number: out of range");
                return false;
            }
            //Valid in row
            int[] toCheck = getRow(field[0]);
            toCheck.SetValue(0, field[1]); //Set variable position to check to 0
            if (toCheck.Contains(value))
            {
                //Console.WriteLine("[TESTING] Invalid number: already in row");
                return false;
            }
            //Valid in column
            toCheck = getColumn(field[1]);
            toCheck.SetValue(0, field[0]); //Set variable position to check to 0
            if (toCheck.Contains(value))
            {
                //Console.WriteLine("[TESTING] Invalid number: already in column");
                return false;
            }
            //Valid in box
            int[] box = { field[0] - (field[0] % 3), field[1] - (field[1] % 3) };
            toCheck = getBox(box);
            int index = field[0] % 3 * 3 + (field[1] % 3);
            toCheck.SetValue(0, index);
            if(toCheck.Contains(value))
            {
                //Console.WriteLine("[TESTING] Invalid number: already in box");
                return false;
            }
            Console.WriteLine("[TESTING] Filling [" + field[0] + ", " + field[1] + "] with: " + value);
            return true;
        }

        private void PrintSudoku()
        {
            for (int i = 0; i < SudokuSize; i++)
            {
                for (int j = 0; j < SudokuSize; j++)
                {
                    Console.Write(Sudoku[i, j] + " ");
                }
                Console.WriteLine();
            }
        }
    }
}
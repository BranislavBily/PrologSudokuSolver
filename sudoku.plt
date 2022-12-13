:- begin_tests(sudoku).
:- include(sudoku).

%Testing same
test(1, [fail]) :- same(2,1).
test(2) :- same(1,1).
test(3) :- same(2,2).
test(4, [fail]) :- same(1,10).

%Testing not same
test(5) :- not_same(2,1).
test(6, [fail]) :- not_same(1,1).
test(7, [fail]) :- not_same(2,2).
test(8) :- not_same(1,10).

%Testing all_numbers_different
test(9) :- all_numbers_different([1,2,3,4,5,6,7,8,9]).
test(10, [fail]) :- all_numbers_different([1,2,3,4,5,6,7,8,9,9]).
test(11, [fail]) :- all_numbers_different([1,2,3,4,5,6,7,8,9,1]).

%Testing first_row_size
test(12) :- first_row_size([[1,2,3,4,5,6,7,8,9], [1,3,4,4,5,6,4,23,1,1000]], 9).
test(13, [fail]) :- first_row_size([[1,2,3,4,5,6,7,8,9]], 10).
test(14) :- first_row_size([], 0).

%Testing check_sudoku_square
test(15, [fail]) :- check_sudoku_square_and_get_height([[1,2,3,4,5,6,7,8,9], [1,3,4,4,5,6,4,23,1,1000]], _).
test(16, [true(X = 3)]) :- check_sudoku_square_and_get_height([[1,2,3], [4,5,6], [7,8,9]], X).
test(17, [fail]) :- check_sudoku_square_and_get_height([[1,2,3], [4,5,6]], _).
test(18, [true(X = 4)]) :- check_sudoku_square_and_get_height([[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]], X).

%Testing transpose
test(19) :- transpose([[1,2,3], [4,5,6], [7,8,9]], [[1,4,7], [2,5,8], [3,6,9]]).
test(20) :- transpose([[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]], [[1,5,9,13], [2,6,10,14], [3,7,11,15], [4,8,12,16]]).
test(21) :- transpose([[1,2,3,4,5,6,7,8,9], 
                    [9,8,7,6,5,4,3,2,1],
                    [1,2,3,4,5,6,7,8,9],
                    [9,8,7,6,5,4,3,2,1],
                    [1,2,3,4,5,6,7,8,9],
                    [9,8,7,6,5,4,3,2,1],
                    [1,2,3,4,5,6,7,8,9],
                    [9,8,7,6,5,4,3,2,1],
                    [1,2,3,4,5,6,7,8,9]],

                   [[1,9,1,9,1,9,1,9,1],
                    [2,8,2,8,2,8,2,8,2],
                    [3,7,3,7,3,7,3,7,3],
                    [4,6,4,6,4,6,4,6,4],
                    [5,5,5,5,5,5,5,5,5],
                    [6,4,6,4,6,4,6,4,6],
                    [7,3,7,3,7,3,7,3,7],
                    [8,2,8,2,8,2,8,2,8],
                    [9,1,9,1,9,1,9,1,9]
                    ]).

%Testing check_sudoku_numbers
test(22) :- check_sudoku_numbers([[1,3,2,4], [4,2,1,3], [3,1,4,2], [2,4,3,1]], 4).
%Number 5 does not belong
test(23, [fail]) :- check_sudoku_numbers([[1,3,2,4], [4,2,1,3], [3,1,4,2], [2,4,3,5]], 4).
%Number 4 appears twice in second row
test(24, [fail]) :- check_sudoku_numbers([[1,3,2,4], [4,2,4,3], [3,1,4,2], [2,4,3,5]], 4).
%Number 1 appears twice in first column
test(25, [fail]) :- check_sudoku_numbers([[1,3,2,4], [1,2,1,3], [3,1,4,2], [2,4,3,5]], 4).

:- end_tests(sudoku).
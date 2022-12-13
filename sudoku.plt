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
test(15, [fail]) :- check_sudoku_square_and_get_size([[1,2,3,4,5,6,7,8,9], [1,3,4,4,5,6,4,23,1,1000]], _).
test(16, [true(X = 3)]) :- check_sudoku_square_and_get_size([[1,2,3], [4,5,6], [7,8,9]], X).
test(17, [fail]) :- check_sudoku_square_and_get_size([[1,2,3], [4,5,6]], _).
test(18, [true(X = 4)]) :- check_sudoku_square_and_get_size([[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]], X).
test(19, [true(X = 4)]) :- check_sudoku_square_and_get_size([[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]], X).

%Testing transpose
test(20) :- transpose([[1,2,3], [4,5,6], [7,8,9]], [[1,4,7], [2,5,8], [3,6,9]]).
test(21) :- transpose([[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]], [[1,5,9,13], [2,6,10,14], [3,7,11,15], [4,8,12,16]]).
test(22) :- transpose([[1,2,3,4,5,6,7,8,9], 
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
test(23) :- check_sudoku_numbers([[1,3,2,4], [4,2,1,3], [3,1,4,2], [2,4,3,1]], 4).
%Number 5 does not belong
test(24, [fail]) :- check_sudoku_numbers([[1,3,2,4], [4,2,1,3], [3,1,4,2], [2,4,3,5]], 4).
%Number 4 appears twice in second row
test(25, [fail]) :- check_sudoku_numbers([[1,3,2,4], [4,2,4,3], [3,1,4,2], [2,4,3,5]], 4).
%Number 1 appears twice in first column
test(26, [fail]) :- check_sudoku_numbers([[1,3,2,4], [1,2,1,3], [3,1,4,2], [2,4,3,5]], 4).

test(27, [all(X == [1])]) :- check_sudoku_numbers([[X,3,2,4], [4,2,X,3], [3,X,4,2], [2,4,3,X]], 4).
test(28, [all(Y == [2])]) :- check_sudoku_numbers([[1,3,Y,4], [4,Y,1,3], [3,1,4,Y], [Y,4,3,1]], 4).

test(29, [true(X = 1)]) :- check_square_dims(1,X).
test(30, [true(X = 2)]) :- check_square_dims(4, X).
test(31, [true(X = 3)]) :- check_square_dims(9, X).
test(32, [true(X = 4)]) :- check_square_dims(16, X).
test(33, [fail]) :- check_square_dims(10, 3.16).

test(34, [true(Y = [4,5])]) :- get_rows_for_sudoku_squares([1,2,3,4,5], 3, [1,2,3], Y).
test(35, [true(Y = [2,3,4,5,6,10])]) :- get_rows_for_sudoku_squares([1,2,3,4,5,6,10], 1, [1], Y).

test(36, [true(FirstTwoRows = [[1,3,2,4], [4,2,1,3]])]) :- get_rows_for_sudoku_squares([[1,3,2,4], [4,2,1,3], [3,1,4,2], [2,4,3,5]], 2, FirstTwoRows, [[3,1,4,2],[2,4,3,5]]).

test(37, [true(X = [1,3,4,2])]) :- create_square([[1,3,2,4], [4,2,1,3]], 2, 2, X, [[2,4], [1,3]]).
test(38, [true(X = [2,4,1,3])]) :- create_square([[2,4], [1,3]], 2, 2, X, [[], []]).

test(39) :- check_every_square_in_given_rows([[2,4], [1,3]], 2, 2, 2).
test(40) :- check_every_square_in_given_rows([[1,3,2,4], [4,2,1,3]], 2, 2, 2).
test(41) :- check_every_square_in_given_rows([
                                            [7,3,4, 1,6,2, 9,8,5],
                                            [6,8,5, 4,7,9, 3,2,1],
                                            [2,1,9, 5,3,8, 6,4,7]
                                            ], 3, 3, 3).

test(42) :- check_squares([[1,3,2,4], [1,2,1,3], [3,1,4,2], [2,4,3,1]], 2).
test(43, [fail]) :- check_squares([[1,3,2,4], [1,2,1,3], [3,1,4,2], [2,4,3,5]], 2).

test(44) :- check_squares([
    [7,3,4, 1,6,2, 9,8,5],
    [6,8,5, 4,7,9, 3,2,1],
    [2,1,9, 5,3,8, 6,4,7],

    [5,6,8, 9,1,3, 2,7,4],
    [3,4,2, 6,8,7, 1,5,9],
    [1,9,7, 2,5,4, 8,3,6],

    [8,5,1, 7,2,6, 4,9,3],
    [9,2,6, 3,4,5, 7,1,8],
    [4,7,3, 8,9,1, 5,6,2]
    ], 3).

test(45, [fail]) :-
    check_squares([
        [7,3,4, 1,6,2, 9,8,5],
        [6,0,5, 4,7,9, 3,2,1],
        [2,1,9, 5,3,8, 6,4,7],
    
        [5,6,8, 9,1,3, 2,7,4],
        [3,4,2, 6,8,7, 1,5,9],
        [1,9,7, 2,5,4, 8,3,6],
    
        [8,5,1, 7,2,6, 4,9,3],
        [9,2,6, 3,4,5, 7,1,8],
        [4,7,3, 8,9,1, 5,6,2]
        ], 3).

test(46, [all(X = [8])]) :- sudoku([
            [7,3,4, 1,6,2, 9,X,5],
            [6,X,5, 4,7,9, 3,2,1],
            [2,1,9, 5,3,X, 6,4,7],
        
            [5,6,X, 9,1,3, 2,7,4],
            [3,4,2, 6,X,7, 1,5,9],
            [1,9,7, 2,5,4, X,3,6],
        
            [X,5,1, 7,2,6, 4,9,3],
            [9,2,6, 3,4,5, 7,1,X],
            [4,7,3, X,9,1, 5,6,2]
            ]).

test(47, [all(X = [8, 1])]) :- sudoku([
            [7,3,4, Y,6,2, 9,X,5],
            [6,X,5, 4,7,9, 3,2,Y],
            [2,Y,9, 5,3,X, 6,4,7],
        
            [5,6,X, 9,Y,3, 2,7,4],
            [3,4,2, 6,X,7, Y,5,9],
            [Y,9,7, 2,5,4, X,3,6],
        
            [X,5,Y, 7,2,6, 4,9,3],
            [9,2,6, 3,4,5, 7,Y,X],
            [4,7,3, X,9,Y, 5,6,2]
            ]).
test(47, [all(Y = [1, 8])]) :- sudoku([
            [7,3,4, Y,6,2, 9,X,5],
            [6,X,5, 4,7,9, 3,2,Y],
            [2,Y,9, 5,3,X, 6,4,7],
        
            [5,6,X, 9,Y,3, 2,7,4],
            [3,4,2, 6,X,7, Y,5,9],
            [Y,9,7, 2,5,4, X,3,6],
        
            [X,5,Y, 7,2,6, 4,9,3],
            [9,2,6, 3,4,5, 7,Y,X],
            [4,7,3, X,9,Y, 5,6,2]
            ]).

test(48, [fail]) :-
    sudoku([
        [7,3,4, Y,6,2, 9,X,5],
        [6,X,5, 4,7,9, 3,X,Y],
        [2,Y,9, 5,3,X, 6,4,7],
    
        [5,6,X, 9,Y,3, 2,7,4],
        [3,4,2, 6,X,7, Y,5,9],
        [Y,9,7, 2,5,4, X,3,6],
    
        [X,5,Y, 7,2,6, 4,9,3],
        [9,2,6, 3,4,5, 7,Y,X],
        [4,7,3, X,9,Y, 5,6,2]
        ]).


:- end_tests(sudoku).
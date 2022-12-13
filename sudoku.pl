:- use_module(library(lists)).

%Returns true if all elements in list are in range
%all_in_range/3(+Lst, +Min, +Max)
in_range(Lst, Min, Max) :-
    maplist(between(Min, Max), Lst).

%Returns true if two "elements" are not the same
%not_same/2(+X, +Y)
not_same(X, Y) :- X \= Y.

%Checks if all numbers in list are different by
%checking every possible combination
%all_numbers_different/1(+Lst)
all_numbers_different([]).
all_numbers_different([H|T]) :-
    maplist(not_same(H), T),
    all_numbers_different(T).

%Checks if size of Row is the same as the Size of the sudoku
%first_row_size/2(+Puzzle, +Size)
first_row_size([], 0).
first_row_size([Row|_], Size) :-
    length(Row, Size).

%Check if the sudoku puzzle is square
%check_sudoku_square_and_get_size/2(+Puzzle, -Size)
check_sudoku_square_and_get_size(Puzzle, Size) :-
    length(Puzzle, Size),
    maplist(same_length(Puzzle), Puzzle),
    first_row_size(Puzzle, Size).

%Nicest solution I found here https://9to5answer.com/how-to-transpose-a-matrix-in-prolog
%I would have used transpose in clpfd but I can't :(
%transpose/2(+Matrix, -Transpose)
transpose(Matrix, Transpose) :-
    nonvar(Matrix),
    findall(L, maplist(nth1(_), Matrix, L), Transpose).

check_sudoku_row(Size, Row) :-
    in_range(Row, 1, Size),
    all_numbers_different(Row).

%Check if all number are in range 1 to Size
%And if all numbers are different
%check_sudoku_numbers/2(+Rows, +Size)
check_sudoku_numbers(Rows, Size) :-
    %Check if all numbers in a row are different
    maplist(check_sudoku_row(Size), Rows),

    transpose(Rows, Cols),
    maplist(check_sudoku_row(Size), Cols).

%Create one NxN square from N rows and "appends" them into one list
%create_square(+NRows, +N, +NumberOfElementsInSquare, -SquareElements, -RestOfRows)
create_square(Rows, 0, _, [], Rows).
create_square([H | Tail], Size, NumberOfElementsInSquare, Square, [Last | RestOfRows]) :-
    get_rows_for_sudoku_squares(H, NumberOfElementsInSquare, FirstN, Last),
    NewSize is Size - 1,
    create_square(Tail, NewSize, NumberOfElementsInSquare, Square2, RestOfRows),
    append(FirstN, Square2, Square), !.

%Checks if all numbers in square are different
%check_every_square_in_given_rows/3(+Rows, +Iter,+SquareSize,  +SudokuSize)
check_every_square_in_given_rows([[], []], _,  _, _) :- !.
check_every_square_in_given_rows(Rows, 1, SquareSize, _) :-
    append(Rows, Numbers),
    SquareRange is SquareSize * SquareSize,
    in_range(Numbers, 1, SquareRange),
    !.

check_every_square_in_given_rows(Rows, Iter, SquareSize,  SudokuSize) :-
    create_square(Rows, SudokuSize, SudokuSize, NumbersInSquare, RestOfRow),
    SquareRange is SquareSize * SquareSize,
    in_range(NumbersInSquare, 1, SquareRange),
    NewIter is Iter -1,
    %call on smaller
    check_every_square_in_given_rows(RestOfRow, NewIter, SquareSize, SudokuSize).


%get_rows_for_sudoku_squares(+Rows, +Size, -FirstNElements, -RestOfRow)
get_rows_for_sudoku_squares(Rows, 0, [], Rows).
get_rows_for_sudoku_squares([H|T], 1, [H], T) :- !.
get_rows_for_sudoku_squares([H|T], Size, [H|FirstNElements], RestOfRow) :-
    Size > 1,
    NewSize is Size - 1,
    get_rows_for_sudoku_squares(T, NewSize, FirstNElements, RestOfRow), !.

%check_squares_in_size_rows(+Rows, +SquareSize, -UncheckedRows)
check_squares_in_size_rows(Rows, SquareSize, UncheckedRows) :-
    get_rows_for_sudoku_squares(Rows, SquareSize, NRows, UncheckedRows),
    check_every_square_in_given_rows(NRows, SquareSize, SquareSize, SquareSize).

%Separates rows into the square size
%check_square(+Rows, +SquareSize)
check_squares([], _) :- !.
check_squares(Rows, SquareSize) :-
    check_squares_in_size_rows(Rows, SquareSize, UncheckedRows),
    check_squares(UncheckedRows, SquareSize).

%Checks if size is power of Natural number
%check_square_dims/2(+Size, -LittleSquareSize)
check_square_dims(Size, LittleSquareSize) :-
    LittleSquareSize is round(sqrt(Size)),
    LittleSquareSize * LittleSquareSize =:= Size.

%Checks is size is power of Natural number and 
%if it is, checks if all numbers in squares are different
%check_sudoku_squares/2(+Rows, +Size)
check_sudoku_squares(Rows, Size) :-
    check_square_dims(Size, SquareSize),

    check_squares(Rows, SquareSize).

%Checks contraints of sudoku, so if it is square, if all numbers are in range
%and if all numbers are different in a row and in a column and in squares
%check_sudoku_contraints/1(+Puzzle)
check_sudoku_contraints(Puzzle) :-
    check_sudoku_square_and_get_size(Puzzle, Size), 

    check_sudoku_numbers(Puzzle, Size),

    check_sudoku_squares(Puzzle, Size).

%Solves sudoku puzzle
%sudoku/1(+Puzzle)
sudoku(Puzzle) :-
    check_sudoku_contraints(Puzzle).
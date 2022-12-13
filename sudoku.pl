:- use_module(library(lists)).
%Returns true if two "elements" are same
%same/2(+X, +Y)
same(X, Y) :- X = Y.

%If P is success, fail and never come back
%notProvable/1(+P)
notProvable(P) :- P, !, fail.
notProvable(_).

%Returns true if two "elements" are not the same
%Yes I could have used \= but I wanted to try not predicate
%not_same/2(+X, +Y)
not_same(X, Y) :- notProvable(same(X, Y)).

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

%Check if all number are in range 1 to Size
%And if all numbers are different
%check_sudoku_numbers/2(+Rows, +Size)
check_sudoku_numbers(Rows, Size) :-
    append(Rows, Numbers),
    %Check if all numbers are in range of 1 to Size
    maplist(between(1, Size), Numbers),
    %Check if all numbers in a row are different
    maplist(all_numbers_different, Rows),

    transpose(Rows, Cols),
    maplist(all_numbers_different, Cols).

%Checks is size is power of Natural number and 
%if it is, checks if all numbers in squares are different
%check_sudoku_squares/2(+Rows, +Size)
check_sudoku_square(Rows, Size) :-
    LittleSquareSize is round(sqrt(Size)),
    LittleSquareSize * LittleSquareSize =:= Size.

    %check_squares(Rows, LittleSquareSize).

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
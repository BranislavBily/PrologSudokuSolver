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

%Checks if size of Row is the same as the Height of the sudoku
%first_row_size/2(+Puzzle, +Height)
first_row_size([], 0).
first_row_size([Row|_], Height) :-
    length(Row, Height).

%Check if the sudoku puzzle is square
%check_sudoku_square_and_get_height/2(+Puzzle, -Height)
check_sudoku_square_and_get_height(Puzzle, Height) :-
    length(Puzzle, Height),
    maplist(same_length(Puzzle), Puzzle),
    first_row_size(Puzzle, Height).

%Nicest solution I found here https://9to5answer.com/how-to-transpose-a-matrix-in-prolog
%I would have used transpose in clpfd but I can't :(
%transpose/2(+Matrix, -Transpose)
transpose(Matrix, Transpose) :-
    nonvar(Matrix),
    findall(L, maplist(nth1(_), Matrix, L), Transpose).

%Check if all number are in range 1 to Height
%And if all numbers are different
%check_sudoku_numbers/2(+Rows, +Height)
check_sudoku_numbers(Rows, Height) :-
    append(Rows, Numbers),
    %Check if all numbers are in range of 1 to Height
    maplist(between(1, Height), Numbers),
    %Check if all numbers in a row are different
    maplist(all_numbers_different, Rows),

    transpose(Rows, Cols),
    maplist(all_numbers_different, Cols).

check_sudoku_contraints(Puzzle) :-
    check_sudoku_square_and_get_height(Puzzle, Height), 

    check_sudoku_numbers(Puzzle, Height),

sudoku(Puzzle) :-
    check_sudoku_contraints(Puzzle).
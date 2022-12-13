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
first_row_size([Row|_], Height) :-
    length(Row, Height).

check_sudoku_contraints(Puzzle) :-
    %Check if the Puzzle is square
    length(Puzzle, Height),
    maplist(same_length(Puzzle), Puzzle),
    first_row_size(Puzzle, Height),

    append(Puzzle, Numbers),
    %Check if all numbers are in range of 1 to Height
    maplist(between(1, Height), Numbers),
    %Check if all numbers are in range of 1 to Height
    maplist(all_numbers_different, Puzzle).

sudoku(Puzzle) :-
    check_sudoku_contraints(Puzzle).
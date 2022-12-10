:- use_module(library(lists)).
%Usage of this is forbidden yey
:- use_module(library(clpfd)).

sudoku(Puzzle) :-
    %Check if the Puzzle is square
    length(Puzzle, Height),
    maplist(same_length(Puzzle), Puzzle),
    first_row_size(Puzzle, Height),

    %Check if all numbers are in range of 1 to Height
    append(Puzzle, Numbers),
    Numbers ins 1..Height,

    %Now check all rows and columns is distinct
    Rows = Puzzle,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),

    %Now check if all numbers in every square is distinct
    check_squares(Rows).

%Checks if size of Row is the same as the Height of the sudoku
%first_row_size/2(+Puzzle, +Height)
first_row_size([Row|_], Height) :-
    length(Row, Height).

%Checks all squares in Sudoku, if every square consists of only unique numbers
%check_squares/1(+Rows)
check_squares(Rows) :-
    Rows = [A, B, C, D, E, F, G, H, I],
    squares(A, B, C),
    squares(D, E, F),
    squares(G, H, I).

%Creates square from each row and checks if all numbers in them are distinct
%squares/3(+Row1, +Row2, Row3)
squares([], [], []).
squares([N1, N2, N3| Row1],
        [N4, N5, N6| Row2],
        [N7, N8, N9| Row3]) :-
            all_distinct([N1, N2, N3, N4, N5, N6, N7, N8, N9]),
            squares(Row1, Row2, Row3).
    
    
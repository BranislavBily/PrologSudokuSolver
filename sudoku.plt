:- begin_tests(sudoku).
:- include(sudoku).

test(1, [fail]) :- same(2,1).
test(2) :- same(1,1).
test(3) :- same(2,2).
test(4, [fail]) :- same(1,10).

:- end_tests(sudoku).
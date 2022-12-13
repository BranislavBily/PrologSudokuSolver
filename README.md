# Sudoku solver

## Minimální požadavky

Vaše aplikace musí umět alespoň:
   - Načíst a interně zareprezentovat sudoku.
   - Najít alespoň jedno řešení (existuje-li) libovolně zadaného sudoku 9x9.

## Extra body

Extra body jsou za následující rozšíření:

    Velikost hrací desky není omezená ({n^2 * n^2∣n≥1}

Naimplementovaná veľkosť hracej dosky n^2 * n^2.
Vstupný predikát sudoku(Puzzle), kde Puzzle je 2d list reprezentujúci hraciu dosku. Čísla môžu byť nahradené premennou, ktorá sa naplní riešením, ak nejaké existuje. Ak ich existuje viac, predikát nájde všetky.


## Testy
Testy sa dajú spustiť pomocou príkazu 
````bash
swipl -t "load_test_files(make(all)), run_tests." -s sudoku.pl
```` 
z terminálu alebo pomocou 
````Prolog
consult("[path]/sudoku.pl"), load_test_files(make(all)), run_tests.
```` 
priamo vo swipl.

## Použitie

Zavoláme predikát ````sudoku(Puzzle)```` , kde Puzzle je 2d list reprezentujúci hraciu dosku. Ak nepoužijeme premenné, predikát vráti, či je herná doska korektná.

````Prolog
sudoku([
    [7,3,4, 1,6,2, 9,8,5],
    [6,8,5, 4,7,9, 3,2,1],
    [2,1,9, 5,3,8, 6,4,7],

    [5,6,8, 9,1,3, 2,7,4],
    [3,4,2, 6,8,7, 1,5,9],
    [1,9,7, 2,5,4, 8,3,6],

    [8,5,1, 7,2,6, 4,9,3],
    [9,2,6, 3,4,5, 7,1,8],
    [4,7,3, 8,9,1, 5,6,2]
    ]).
true.
````

Ak hracia doska nie je korektná, vráti false. (V tomto prípade kvôli 0.)

````Prolog
sudoku([
    [7,3,4, 1,6,2, 9,8,5],
    [6,8,5, 4,7,9, 3,2,1],
    [2,1,9, 5,3,8, 6,4,7],

    [5,6,8, 9,1,3, 2,7,4],
    [3,4,2, 6,0,7, 1,5,9],
    [1,9,7, 2,5,4, 8,3,6],

    [8,5,1, 7,2,6, 4,9,3],
    [9,2,6, 3,4,5, 7,1,8],
    [4,7,3, 8,9,1, 5,6,2]
    ]).
false.
````

Ak hraciu dosku naplníme premennými, predikát vráti všetky riešenia sudoku, ak nejaké existujú.
````Prolog
sudoku([
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
Y = 1,
X = 8 ;
Y = 8,
X = 1 ;
false.
````

Samozrejme môžeme to pre predikát trochu skomplikovať pridaním premenných.

````Prolog
sudoku([
            [Q, 1, 8, W, 9, 6, 7, E, R],
            [4, 9, 6, 1, 5, 7, T, Y, 2],
            [U, 5, 3, I, O, 4, 1, 9, P],
            [1, 8, 5, 7, 6, A, 4, 2, S],
            [3, 7, 4, D, 2, 8, F, 6, G],
            [9, 6, 2, 4, 1, 5, 3, H, J],
            [5, 3, 1, 6, 7, 2, K, L, 4],
            [6, 4, 9, 8, 3, 1, Z, 5, 7],
            [8, 2, 7, 5, 4, 9, X, 1, 3]
            ]).
Q = I, I = Z, Z = 2,
W = Y, Y = A, A = 3,
E = 4,
R = F, F = 5,
T = O, O = J, J = L, L = 8,
U = H, H = 7,
P = X, X = 6,
S = D, D = K, K = 9,
G = 1 ;
false.
````

Alebo zväčšením hracej dosky.

````Prolog
%Korektná hracia doska 16x16
sudoku([
        [4,10,9,15,1,7,13,8,6,14,2,12,16,5,3,11],
        [2,5,3,1,15,4,11,16,13,9,8,7,6,10,12,14],
        [14,6,13,12,3,10,5,2,16,11,1,4,8,15,9,7],
        [11,7,16,8,6,14,9,12,5,3,10,15,1,2,13,4],
        [8,16,11,4,13,15,14,9,2,5,7,3,12,1,10,6],
        [1,14,6,13,12,8,4,5,10,16,9,11,2,3,7,15],
        [10,15,5,3,2,1,6,7,4,12,14,8,9,11,16,13],
        [12,2,7,9,11,3,16,10,15,13,6,1,4,8,14,5],
        [9,4,1,10,14,2,3,13,11,15,12,6,7,16,5,8],
        [5,8,14,16,7,9,1,6,3,4,13,10,11,12,15,2],
        [7,3,15,6,16,11,12,4,8,2,5,9,14,13,1,10],
        [13,12,2,11,10,5,8,15,7,1,16,14,3,6,4,9],
        [15,9,8,2,4,12,7,3,1,10,11,13,5,14,6,16],
        [6,13,12,5,9,16,15,1,14,8,4,2,10,7,11,3],
        [16,11,4,7,8,13,10,14,12,6,3,5,15,9,2,1],
        [3,1,10,14,5,6,2,11,9,7,15,16,13,4,8,12]
        ]).
true.
````
Pridáme premenné a necháme predikát vyriešiť puzzle.
````Prolog
sudoku([
        [4,10,9,15,1,7,13,8,6,14,2,12,16,Q,3,11],
        [2,5,3,1,W,4,11,16,13,9,8,7,6,10,12,14],
        [14,6,13,12,3,10,E,2,16,11,R,4,8,15,9,7],
        [11,7,16,8,6,14,9,12,5,3,10,15,1,2,13,4],
        [T,16,11,4,13,15,Y,9,2,5,7,3,12,1,10,6],
        [1,14,6,13,12,8,4,5,U,16,9,11,2,I,7,15],
        [O,15,5,3,2,1,6,7,4,12,14,8,9,11,P,13],
        [12,2,7,9,11,3,A,10,15,13,6,1,4,8,14,5],
        [9,4,1,S,14,2,3,13,11,15,D,6,7,16,5,8],
        [5,F,14,16,7,9,1,6,3,4,13,10,11,G,15,2],
        [7,3,15,6,16,H,12,4,8,2,5,9,14,13,1,10],
        [13,12,2,11,10,5,8,15,J,1,16,14,K,6,4,9],
        [15,9,L,2,4,12,Z,3,1,10,11,13,5,14,6,16],
        [6,13,12,5,9,16,15,1,14,8,4,2,10,7,11,3],
        [16,X,4,7,8,13,10,14,C,6,3,5,15,9,2,1],
        [3,1,10,V,5,6,2,11,9,7,15,16,13,4,8,12]
        ]).
Q = E, E = 5,
W = 15,
R = 1,
T = F, F = L, L = 8,
Y = V, V = 14,
U = O, O = S, S = 10,
I = K, K = 3,
P = A, A = 16,
D = G, G = C, C = 12,
H = X, X = 11,
J = Z, Z = 7 ;
false.
````

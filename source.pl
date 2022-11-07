% page three states all modes should work, this are the tree modes we will use.
% ? instantiaded or not, input/output use mostly this.
% + instantited > input
% - uninstatiated > output

% Load library's
%:- use_module( [library(lists), library( 'io' ), library( 'fill' )] ).
:- use_module([library(lists), io, fill]).

% Board representation. 
% There are eight ways to make a line of pieces > "Programming in Prolog p 93"

% x is 1 2 or 3, list position 1,4,7.
% y is 1,2 or 3, list position 2,3, 5,6, 8,9


% is_cross/1 succeeds when its argument is the cross character in the representation.
is_cross( Cross ) :- 
    Cross = x.

% is_nought/1 succeeds when its argument is the nought character in the representation.

is_nought( Nought ) :-
    Nought = o.

% is_empty/1 succeeds when its argument is the empty square character in the representation.

is_empty( Empty ) :-
    Empty = e.

% is_piece/1 succeeds when its argument is either the cross character or the nought character.

is_piece( Piece ) :-
    is_nought( Piece ).
is_piece( Piece ) :-
    is_cross( Piece ).

%is_board_representation( Board ):-
 %   Board = (row(N1, A1, B1, C1), row(N2, A2, B2, C2), row(N3, A3, B3, C3)).

% other_player/2 succeeds when both its arguments are player representation characters, but they are different.

other_player( P1, P2) :-
    \+ (P1 = P2),
    is_piece( P1 ),
    is_piece( P2 ).
   

/* row/3 succeeds when its first argument is a row number (between 1 and 3) and its second
is a representation of a board state. The third argument will then be a term like this:
row( N, A, B, C ), where N is the row number, and A, B, C are the values of the
squares in that row. */

row(Rn, Board, row(N,A,B,C)) :-
    % No check to see if Rn is beteen 1 and 3 since, if not then memebr will return false.
    N=Rn,
    member(row(N,A,B,C),Board).

/*column/3 succeeds when its first argument is a column number (between 1 and 3) and its
second is a representation of a board state. The third argument will then be a term like
this: col( N, A, B, C ), where N is the column number, and A, B, C are the values
of the squares in that column.*/

column(Cn, Board, col(N,A,B,C)) :-
    N=Cn,
    member(col(N,A,B,C),Board).

/*diagonal/3 succeeds when its first argument is either top to bottom or bottom to top and
its second is a representation of a board state. The third argument will then be a
term like this: dia( D, A, B, C ), where D is the direction of the line (as above),
and A, B, C are the values of the squares in that diagonal. The diagonal direction (eg
top-to-bottom) is moving from left to right.*/

diagonal(Dn, Board, dia(N,A,B,C)) :-
    N=Dn,
    member(dia(N,A,B,C),Board).

/*square/4 succeeds when its first two arguments are numbers between 1 and 3, and its third
is a representation of a board state. The fourth argument will then be a term like this:
squ( X, Y, Piece ), where (X,Y) are the coordinates of the square given in the first
two arguments, and Piece is one of the three square representation characters, indicating
what if anything occupies the relevant square.*/

square(X, Y, Board, squ( X, Y, Piece)) :-
    Y = 1, % test if y is 1, fail otherwise.
    squ(X,Y,A) = member(row(X,A,B,C),Boad).
    %row(N, A, B, C) = row(X, Board, row(N,A,B,C)),
   % squ(X,Y,Piece) = squ(X,Y,A).

    %member(squ(X,Y,Piece),[squ(X,Y, A)]).


/*empty_square/3 succeeds when its first two arguments are coordinates on the board, and the
square they name is empty.*/

% X is row number, Y is column number
empty_square(X, Y, Board):-
    member((X, Y, n), Board).


/*initial_board/1 succeeds when its argument represents the initial state of the board.
the initial board has nine empty squares*/

initial_board(Board):-
    Board = [(1,1,n),(1,2,n),(1,3,n),(2,1,n),(2,2,n),(2,3,n),(3,1,n),(3,2,n),(3,3,n)].

/* empty_board/1 succeeds when its argument represents an uninstantiated board (ie with Pro-
log variables in all the spaces). */

empty_board(Board):-
    Board = [(1,(1,_),(2,_),(3,_)),(2,1,_),(2,2,_),(2,3,_),(3,1,_),(3,2,_),(3,3,_)].

%%% TEST CODE %%%

% true if row number exist.
test_row(row, first_row):-
    row(1,[row(1,x,n,o),row(2,x,x,o),row(3,n,o,n)],row(N,A,B,C)).

% true if row number doesn't exist
test_row(row, wrong_row):-
    \+ row(5, [row(1,x,n,o),row(2,x,x,0),row(3,n,o,n)], row(N, A, B, C)).   

% groups both tests above in one.    
tests_row(row, all_tests):-
    test_row(row, first_row),
    test_row(row, wrong_row).

% true if column number exist.
test_column(column, first_column):-
    column(1,[col(1,x,x,n),col(2,n,x,o),col(3,o,o,n)],col(N,A,B,C)).

% true if col number doesn't exist
test_column(column, wrong_column):-
    \+ column(5, [col(1,x,x,n),col(2,n,x,o),col(3,o,o,n)], col(N, A, B, C)). 

% true if diagonal "number" exist.
test_diagonal(diagonal, top-to-bottom):-
    diagonal(top-to-bottom,[dia(top-to-bottom,x,x,n),dia(bottom-to-top, n,x,o)], dia(N,A,B,C)).

% true if diagonal "number" doesn'texist.
test_diagonal(diagonal, top-to-top):-
    diagonal(top-to-bottom,[dia(top-to-bottom,x,x,n),dia(bottom-to-top, n,x,o)], dia(N,A,B,C)).




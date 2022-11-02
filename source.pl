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

is_board_representation( Board ):-
    Board = (row(N1, A1, B1, C1), row(N2, A2, B2, C2), row(N3, A3, B3, C3)).

% other_player/2 succeeds when both its arguments are player representation characters, but they are different.

other_player( P1, P2) :-
    \+ (P1 = P2),
    is_piece( P1 ),
    is_piece( P2 ).
   

/* row/3 succeeds when its first argument is a row number (between 1 and 3) and its second
is a representation of a board state. The third argument will then be a term like this:
row( N, A, B, C ), where N is the row number, and A, B, C are the values of the
squares in that row. */

% the following to bridge my presentation of a board with the library.
% use recursion here
row(Rn, Bs, row(N, A, B, C)):-
    row(Rn,[],_).
row(Rn, Bs, row(N, A, B, C)):-
    row(Rn, [(Rn, 1, A)|Tail], row(Rn,A,_,_)).
row(Rn, Bs, row(N, A, B, C)):-
    row(Rn, [(Rn, 2, B)|Tail], row(Rn,_,B,_)).
row(Rn, Bs, row(N, A, B, C)):-
    row(Rn, [(Rn, 3, C)|Tail], row(Rn,_,_,C)).

    


/*column/3 succeeds when its first argument is a column number (between 1 and 3) and its
second is a representation of a board state. The third argument will then be a term like
this: col( N, A, B, C ), where N is the column number, and A, B, C are the values
of the squares in that column.*/

column(Cm, Bs, col(N,A,B,C)):-
    Cn < 4, Cn > 0,
    Bs.

/*diagonal/3 succeeds when its first argument is either top to bottom or bottom to top and
its second is a representation of a board state. The third argument will then be a
term like this: dia( D, A, B, C ), where D is the direction of the line (as above),
and A, B, C are the values of the squares in that diagonal. The diagonal direction (eg
top-to-bottom) is moving from left to right.*/

/*square/4 succeeds when its first two arguments are numbers between 1 and 3, and its third
is a representation of a board state. The fourth argument will then be a term like this:
squ( X, Y, Piece ), where (X,Y) are the coordinates of the square given in the first
two arguments, and Piece is one of the three square representation characters, indicating
what if anything occupies the relevant square.*/

/*empty_square/3 succeeds when its first two arguments are coordinates on the board, and the
square they name is empty.*/

% X is row number, Y is column number
empty_square(X, Y, Board):-
    member((X, Y, e), Board).


/*initial_board/1 succeeds when its argument represents the initial state of the board.
the initial board has nine empty squares*/

initial_board(Board):-
    Board = [(1,1,e),(1,2,e),(1,3,e),(2,1,e),(2,2,e),(2,3,e),(3,1,e),(3,2,e),(3,3,e)].

/* empty_board/1 succeeds when its argument represents an uninstantiated board (ie with Pro-
log variables in all the spaces). */

empty_board(Board):-
    Board = [(1,1,_),(1,2,_),(1,3,_),(2,1,_),(2,2,_),(2,3,_),(3,1,_),(3,2,_),(3,3,_)].




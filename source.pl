% Load library's
%:- use_module( [library(lists), library( 'io' ), library( 'fill' )] ).
:- use_module([library(lists), io, fill]).

% Board representation. 
% There are eight ways to make a line of pieces > "Programming in Prolog p 93"


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


% other_player/2 succeeds when both its arguments are player representation characters, but they are different.

other_player( P1, P2) :-
    \+ (P1 = P2),
    is_cross(P1),
    is_nought(P2),
    is_cross(P2),
    is_nought(P1).

/* row/3 succeeds when its first argument is a row number (between 1 and 3) and its second
is a representation of a board state. The third argument will then be a term like this:
row( N, A, B, C ), where N is the row number, and A, B, C are the values of the
squares in that row. */

row(Rn, Bs, row(N, A, B, C)).

/*column/3 succeeds when its first argument is a column number (between 1 and 3) and its
second is a representation of a board state. The third argument will then be a term like
this: col( N, A, B, C ), where N is the column number, and A, B, C are the values
of the squares in that column.*/

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

empty_square(X, Y, Board):-
    Board = e.


/*initial_board/1 succeeds when its argument represents the initial state of the board.*/

initial_board(Board):-
    Board.

/* empty_board/1 succeeds when its argument represents an uninstantiated board (ie with Pro-
log variables in all the spaces). */




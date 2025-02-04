% Load library's
:- use_module([library(lists), io, fill]).

% is_cross(?Cross)
% is_cross/1 succeeds when its argument is the cross character in the representation.

is_cross( Cross ) :- 
    Cross = x.

% is_nought(?Nought)
% is_nought/1 succeeds when its argument is the nought character in the representation.

is_nought( Nought ) :-
    Nought = o.

%is_empty(?Empty)
% is_empty/1 succeeds when its argument is the empty square character in the representation.

is_empty( Empty ) :-
    Empty = n.

% is_piece(?Piece)
% is_piece/1 succeeds when its argument is either the cross character or the nought character.

is_piece( Piece ) :-
    is_nought( Piece ).
is_piece( Piece ) :-
    is_cross( Piece ).

% other_player(?P1, ?P2)
% other_player/2 succeeds when both its arguments are player representation characters,
% but they are different.

other_player( P1, P2) :-
    \+ (P1 = P2),
    is_piece( P1 ),
    is_piece( P2 ).
   
/*
row(?Rn, ?Board, row(?N, ?A, ?B, ?C))
row/3 succeeds when its first argument is a row number (between 1 and 3) and its second
is a representation of a board state. The third argument will then be a term like this:
row( N, A, B, C ), where N is the row number, and A, B, C are the values of the
squares in that row. 
*/

row(Rn, Board, row(N,A,B,C)) :-
    % No check to see if Rn is beteen 1 and 3 since, if not then member will return false.
    N=Rn,
    member(row(N,A,B,C),Board).

/*
column(?Cn, ?Board, col(?N, ?A, ?B, ?C))
column/3 succeeds when its first argument is a column number (between 1 and 3) and its
second is a representation of a board state. The third argument will then be a term like
this: col( N, A, B, C ), where N is the column number, and A, B, C are the values
of the squares in that column.
*/

column(Cn, Board, col(N,A,B,C)):-
    Cn = 1,
    N = Cn,
    row(1, Board, row(1,A,_,_)),
    row(2, Board, row(2,B,_,_)),
    row(3, Board, row(3,C,_,_)).

column(Cn, Board, col(N,A,B,C)):-
    Cn = 2,
    N = Cn,
    row(1, Board, row(1,_,A,_)),
    row(2, Board, row(2,_,B,_)),
    row(3, Board, row(3,_,C,_)).

column(Cn, Board, col(N,A,B,C)):-
    Cn = 3,
    N = Cn,
    row(1, Board, row(1,_,_,A)),
    row(2, Board, row(2,_,_,B)),
    row(3, Board, row(3,_,_,C)).


/*
diagonal(?D, ?Board, dia(?N, ?A, ?B, ?C)) 
diagonal/3 succeeds when its first argument is either top-to-bottom or bottom-to-top and
its second is a representation of a board state. The third argument will then be a
term like this: dia( D, A, B, C ), where D is the direction of the line (as above),
and A, B, C are the values of the squares in that diagonal. The diagonal direction (eg
top-to-bottom) is moving from left to right.
*/

diagonal(D, Board, dia(N,A,B,C)) :-
    D = top-to-bottom,
    N = D,
    row(1, Board, row(1,A,_,_)),
    row(2, Board, row(2,_,B,_)),
    row(3, Board, row(3,_,_,C)).

diagonal(D, Board, dia(N,A,B,C)) :-
    D = bottom-to-top,
    N = D,
    row(1, Board, row(1,_,_,C)),
    row(2, Board, row(2,_,B,_)),
    row(3, Board, row(3,A,_,_)).

/*
square(?X, ?Y, ?Board, squ(?X, ?Y, ?Piece))
square/4 succeeds when its first two arguments are numbers between 1 and 3, and its third
is a representation of a board state. The fourth argument will then be a term like this:
squ( X, Y, Piece ), where (X,Y) are the coordinates of the square given in the first
two arguments, and Piece is one of the three square representation characters, indicating
what if anything occupies the relevant square.
*/

% X is tested and can only be 1,2 or 3, 
% Y is implicitly tested when calling row, If Y is not 1,2 or 3, row returns false.
square(X, Y, Board, squ( X, Y, Piece)) :-
    X = 1, 
    row(Y, Board, row(Y,Piece,_,_)).

square(X, Y, Board, squ( X, Y, Piece)) :-
    X = 2,
    row(Y, Board, row(Y,_,Piece,_)).

square(X, Y, Board, squ( X, Y, Piece)) :-
    X = 3, 
    row(Y, Board, row(Y,_,_,Piece)).

/*
empty_square(?X, ?Y, ?Board)
empty_square/3 succeeds when its first two arguments are coordinates on the board, 
and the square they name is empty, empty is represented by n (nothing).
*/

empty_square(X, Y, Board):-
    square(X, Y, Board, squ(X,Y,n)).

/*
initial_board(?Board)
initial_board/1 succeeds when its argument represents the initial state of the board.
the initial board has nine empty squares represented by n (nothing).
*/

initial_board(Board):-
    Board = [row(1,n,n,n),row(2,n,n,n),row(3,n,n,n)].

/* 
empty_board(?Board)
empty_board/1 succeeds when its argument represents an uninstantiated board 
(ie with Prolog variables in all the spaces). 
*/

empty_board(Board):-
    Board = [row(1,_,_,_),row(2,_,_,_),row(3,_,_,_)].

/*
and_the_winner_is(?Board, ?Player)
and the winner is/2 succeeds when its first argument represents a board, and the second is
a player who has won on that board. 
*/

and_the_winner_is(Board, Player):-
    row(_, Board, row(_, Player, Player, Player)).

and_the_winner_is(Board, Player):-
    column(_, Board, col(_, Player, Player, Player)).

and_the_winner_is(Board, Player):-
    diagonal(_, Board, dia(_, Player, Player, Player)).


/*
no_more_free_squares(?Board)
no more free squares/1 succeeds if the board represented in its argument
has no empty squares in it.
*/
 
no_more_free_squares(Board):-
    \+ empty_square(1,1,Board),
    \+ empty_square(1,2,Board),
    \+ empty_square(1,3,Board),
    \+ empty_square(2,1,Board),
    \+ empty_square(2,2,Board),
    \+ empty_square(2,3,Board),
    \+ empty_square(3,1,Board),
    \+ empty_square(3,2,Board),
    \+ empty_square(3,3,Board).

%% 3.6 Running a game for 2 human players %%

playHH :-
    welcome,
    initial_board( Board ),
    display_board( Board ),
    is_cross( Cross ),
    playHH( Cross, Board ).

/*
change_player(?Player, ?NewPlayer)
change_player/2 succeeds when argument one and two are valid pieces but not equal.
given Player, NewPlayer will be the other valid Piece.
*/

change_player(Player, NewPlayer):-
    is_cross(Player),
    is_nought(NewPlayer).

change_player(Player, NewPlayer):-
    is_nought(Player),
    is_cross(NewPlayer).

/*
playHH(?Player, ?Board)
playHH/2 is recursive. It has two arguments: a player, the first, and a board state, the
second. For this section of the practical, it has three possibilities:
*/

% 1. The board represents a winning state, and we have to report the winner. 
playHH(Player, Board):-
    and_the_winner_is(Board, Player),
    report_winner(Player). 

% 2. There are no more free squares on the board, and we have to report a stalemate.
playHH(_, Board):-
    no_more_free_squares(Board),
    report_stalemate.

% 3. We can get a (legal) move from the player named in argument 1, fill the square
% he or she gives, switch players, display the board and then play again, 
% with the updated board and the new player.
playHH(Player, Board):-
    get_legal_move( Player, X, Y, Board ),
    fill_square( X, Y, Player, Board, NewBoard ),
    change_player(Player, NewPlayer),
    display_board(NewBoard),
    playHH(NewPlayer, NewBoard).

%% 4 Running a game for 1 human and the computer %%

playHC :-
    welcome,
    initial_board( Board ),
    display_board( Board ),
    is_nought( Nought ),
    playHC( Nought, Board ).

/*
playHC(?Player, ?Board)
playHC/2 is recursive. It has two arguments: a player, the first, and a board state, the
second. There are three possibilities:
*/

% 1. The board represents a winning state, and we have to report the winner. 
playHC(Player, Board):-
    and_the_winner_is(Board, Player),
    report_winner(Player). 

% 2. There are no more free squares on the board, and we have to report a stalemate.
playHC(_, Board):-
    no_more_free_squares(Board),
    report_stalemate.

/*
3.1 The current player is x, we can get a (legal) move, fill the square, display the board,
and play again, with the new board and with nought as player.
*/

playHC(Player, Board):-
    is_cross(Player),
    get_legal_move( Player, X, Y, Board ),
    fill_square( X, Y, Player, Board, NewBoard ),
    change_player(Player, NewPlayer),
    display_board(NewBoard),
    playHC(NewPlayer, NewBoard).

/*
3.2 The current player is o, we can choose a move (see below), we tell the user what
move we’ve made (see io library), we can fill the square, display the board, and
play again, with the new board and with cross as player.
*/

playHC(Player, Board):-
    is_nought(Player),
    choose_move(Player, X, Y, Board),
    report_move( Player, X, Y ),
    fill_square( X, Y, Player, Board, NewBoard ),
    change_player(Player, NewPlayer),
    display_board(NewBoard),
    playHC(NewPlayer, NewBoard).

%% 5 Implementing the heuristics %%

% 1. If there is a winning line for self, then take it;

% winning horizontal line for player?
choose_move(Player, X, Y, Board):-
    X = 1,
    member(row(Y,n,Player,Player),Board).

choose_move(Player, X, Y, Board):-
    X = 2,
    member(row(Y,Player,n,Player),Board).

choose_move(Player, X, Y, Board):-
    X = 3,
    member(row(Y,Player,Player,n),Board).

% winning vertical line for player? 
choose_move(Player, X, Y, Board):-
    Y = 1,
    column(X, Board, col(X,n,Player,Player)).

choose_move(Player, X, Y, Board):-
    Y = 2,
    column(X, Board, col(X,Player,n,Player)).

choose_move(Player, X, Y, Board):-
    Y = 3,
    column(X, Board, col(X,Player,Player,n)).

% winning diagonal line for player?
choose_move(Player, X, Y, Board):-
    X = 1,
    Y = 1,
    D = top-to-bottom,
    diagonal(D, Board, dia(D,n,Player,Player)).

choose_move(Player, X, Y, Board):-
    X = 2,
    Y = 2,
    D = top-to-bottom,
    diagonal(D, Board, dia(D,Player,n,Player)).

choose_move(Player, X, Y, Board):-
    X = 3,
    Y = 3,
    D = top-to-bottom,
    diagonal(D, Board, dia(D,Player,Player,n)).

choose_move(Player, X, Y, Board):-
    X = 1,
    Y = 3,
    D = bottom-to-top,
    diagonal(D, Board, dia(D,n,Player,Player)).

choose_move(Player, X, Y, Board):-
    X = 2,
    Y = 2,
    D = bottom-to-top,
    diagonal(D, Board, dia(D,Player,n,Player)).

choose_move(Player, X, Y, Board):-
    X = 3,
    Y = 1,
    D = bottom-to-top,
    diagonal(D, Board, dia(D,Player,Player,n)).

% 2. If there is a winning line for opponent, then block it;

% winning horizontal line for opponent? 
choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 1,
    member(row(Y,n,Opponent,Opponent),Board).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 2,
    member(row(Y,Opponent,n,Opponent),Board).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 3,
    member(row(Y,Opponent,Opponent,n),Board).

% winning vertical line for opponent? 
choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    Y = 1,
    column(X, Board, col(X,n,Opponent,Opponent)).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    Y = 2,
    column(X, Board, col(X,Opponent,n,Opponent)).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    Y = 3,
    column(X, Board, col(X,Opponent,Opponent,n)).

% winning diagonal line for Opponent?
choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 1,
    Y = 1,
    D = top-to-bottom,
    diagonal(D, Board, dia(D,n,Opponent,Opponent)).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 2,
    Y = 2,
    D = top-to-bottom,
    diagonal(D, Board, dia(D,Opponent,n,Opponent)).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 3,
    Y = 3,
    D = top-to-bottom,
    diagonal(D, Board, dia(D,Opponent,Opponent,n)).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 1,
    Y = 3,
    D = bottom-to-top,
    diagonal(D, Board, dia(D,n,Opponent,Opponent)).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 2,
    Y = 2,
    D = bottom-to-top,
    diagonal(D, Board, dia(D,Opponent,n,Opponent)).

choose_move(Player, X, Y, Board):-
    change_player(Player, Opponent),
    X = 3,
    Y = 1,
    D = bottom-to-top,
    diagonal(D, Board, dia(D,Opponent,Opponent,n)).

% 3. If the middle space is free, then take it;
choose_move(_, X,Y,Board):-
    X = 2, % (2,2) is the middle space on the board.
    Y = 2,
    empty_square(X,Y, Board).

% 4. If there is a corner space free, then take it;
choose_move(_, X,Y,Board):-
    X = 3,
    Y = 3,
    empty_square(X,Y, Board).

choose_move(_, X,Y,Board):-
    X = 3,
    Y = 1,
    empty_square(X,Y, Board).

choose_move(_, X,Y,Board):-
    X = 1,
    Y = 3,
    empty_square(X,Y, Board).

choose_move(_, X,Y,Board):-
    X = 3,
    Y = 1,
    empty_square(X,Y, Board).

% 5. Otherwise, dumbly choose the next available space.
choose_move( _, X, Y, Board ):-
    empty_square( X, Y, Board ).

% 5.1 Spotting a stalemate.

playSS :-
    welcome,
    initial_board( Board ),
    display_board( Board ),
    is_nought( Nought ),
    playSS( Nought, Board ).

% 1. The board represents a winning state, and we have to report the winner. 
playSS(Player, Board):-
    and_the_winner_is(Board, Player),
    report_winner(Player). 

% 2. If no possible win is available, report a stalemate. 
playSS(Player, Board):-
   \+ possible_win(Player, Board),
   report_stalemate.

% 3. The current player is x, we can get a (legal) move, fill the square, display the board,
% and play again, with the new board and with nought as player.

playSS(Player, Board):-
    is_cross(Player),
    get_legal_move( Player, X, Y, Board ),
    fill_square( X, Y, Player, Board, NewBoard ),
    change_player(Player, NewPlayer),
    display_board(NewBoard),
    playSS(NewPlayer, NewBoard).

/*
3.2 The current player is o, we can choose a move (see below), we tell the user what
move we’ve made (see io library), we can fill the square, display the board, and
play again, with the new board and with cross as player.
*/

playSS(Player, Board):-
    is_nought(Player),
    choose_move(Player, X, Y, Board),
    report_move( Player, X, Y ),
    fill_square( X, Y, Player, Board, NewBoard ),
    change_player(Player, NewPlayer),
    display_board(NewBoard),
    playHC(NewPlayer, NewBoard).

/*
possible_win(?Player, ?Board)
possible win/2 is recursive. In total, it succeeds if any player
can win from the current position, allowing for whose move it is now.
This succeeds if the addition of one square to the board (represented in the second argument)
yields a win for a player (represented in the first argument).
it succeeds if the result of adding one square to the board leads to a possible win, 
swapping players as it goes.
*/

possible_win(Player, Board):-
    choose_move(Player, X,Y, Board),
    fill_square( X, Y, Player, Board, NewBoard ),
    and_the_winner_is(NewBoard, Player),
    change_player(Player, Opponent),
    possible_win(Opponent, NewBoard).

% this will makes possible_win return true at all times, and playSS runs.
% the predicate above does not work correctly 
% (mostly fails, only works when last the last step of the game has to be checked).
possible_win(_, _).
    
 
%%% only TEST CODE underneath.. %%%

test_possible_win():-
    possible_win(x,[row(1,n,x,x),row(2,o,x,o),row(3,o,x,o)]).

% player in argument one is different form player in argument 2.
test_change_player(test):-
    change_player(x,o),
    change_player(o,x).

% testing for winner in a row, column or diagonal.
test_and_the_winner_is(winner, all_tests):-
    test_and_the_winner_isro(winner, row),
    test_and_the_winner_isrx(winner, row),
    test_and_the_winner_isco(winner, column),
    test_and_the_winner_isdx(winner, diagonal).

test_and_the_winner_isro(winner, row):-
    and_the_winner_is([row(1,n,n,n),row(2,o,o,o),row(3,n,n,n)], o).

test_and_the_winner_isrx(winner, row):-
    \+ and_the_winner_is([row(1,n,n,n),row(2,o,o,o),row(3,n,n,n)], x).

test_and_the_winner_isco(winner, column):-
    and_the_winner_is([row(1,n,x,o),row(2,n,n,o),row(3,n,n,o)], o).

test_and_the_winner_isdx(winner, diagonal):-
    and_the_winner_is([row(1,o,x,x),row(2,o,x,n),row(3,x,n,n)], x).

% for value X and Y the piece on the board is given.
test_square():-
    square(1,1,[row(1,x,x,o),row(2,o,x,o),row(3,n,o,x)], squ(1,1,x)).

% groups both tests above in one.    
tests_row_column_diagonal(row, all_tests):-
    test_row(row, first_row),
    test_row(row, wrong_row),
    test_column(column, first_column),
    test_column(column, wrong_column),
    test_diagonal(diagonal, top-to-bottom),
    test_diagonal(diagonal, bottom-to-top).

% true if row number exist.
test_row(row, first_row):-
    row(1,[row(1,x,n,o),row(2,x,x,o),row(3,n,o,n)],row(1,x,n,o)).

% true if row number doesn't exist
test_row(row, wrong_row):-
    \+ row(5, [row(1,x,n,o),row(2,x,x,0),row(3,n,o,n)], row(5, x, o, n)).   

% true if column number exist.
test_column(column, first_column):-
    column(1,[row(1,x,n,o),row(2,x,x,o),row(3,n,o,n)],col(1,x,x,n)).

% true if col number doesn't exist
test_column(column, wrong_column):-
    \+ column(5, [row(1,x,n,o),row(2,x,x,o),row(3,n,o,n)], col(5, x,o,n)). 

% true if diagonal "number" exist.
test_diagonal(diagonal, top-to-bottom):-
    diagonal(top-to-bottom,[row(1,x,n,o),row(2,x,x,o),row(3,n,o,n)], dia(top-to-bottom,x,x,n)).

% true if diagonal "number" doesn'texist.
test_diagonal(diagonal, bottom-to-top):-
    \+ diagonal(bottom-to-top,[row(1,x,n,o),row(2,x,x,o),row(3,n,o,n)], dia(top-to-top,x,x,o)).




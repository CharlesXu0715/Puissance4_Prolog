% Copyright 2016 Ramon ViÃ±as, Marc Roig
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

% ------------------------- Original code ----------------------------

%%%%%%%%%%%%%%%%%%
%%%%% BOARD %%%%%%
%%%%%%%%%%%%%%%%%%
%Initialize empty board (matrix of dimensions [columns=7, rows=6]. This board representation will make gameplay easier than if we used [rows, columns])
initial(board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-']])).

%%%%%%%%%%%%%%%%%%
%%% SHOW BOARD %%%
%%%%%%%%%%%%%%%%%%
%show(X) shows board X
show(board(X)):- write('  A B C D E F G'), nl,
		 iShow(X,6).

%show(X,N) shows lines [N .. 1] of board X
iShow(_,0).
iShow(X,N):- showLine(X,N,X2),
	     Ns is N-1,
	     iShow(X2,Ns).

%showLine(X,N,X2) writes N and shows first line of board X (first element of every column). X2 is X without the shown line.
showLine(X,N,X2):- write(N), write(' '),
		   iShowLine(X,X2), nl.

%iShowLine(X,X2) writes first element of every column. X2 is X without the shown line.
iShowLine([],_).
iShowLine([[X|X2]|XS],[X2|XS2]):- write(X), write(' '),
			          iShowLine(XS,XS2).

%%%%%%%%%%%%%%%%%%
%%%% GAMEPLAY %%%%
%%%%%%%%%%%%%%%%%%
% Initializes board and starts the game
% Uses H to choose the heuristic for the machine
connect4(H):- initial(X),
	   show(X),
	   nextMove('X',X,H), !.

%nextMove(J,X) J is the player that needs to move ('O' or 'X') and X is the board. Checks if the game has finished. If it hasn't finished, performs next move.
nextMove('X',X,_):- wins('O',X),
		  write('Machine wins!').
nextMove('O',X,_):- wins('X',X),
		  write('You win!').
nextMove(_,X,_):- full(X),
		write('Draw').
nextMove('X',X,H):- repeat, %repeats in case a column is full
		  readColumn(C),
		  play('X',C,X,X2), !,
		  show(X2),
		  nextMove('O',X2,H).
nextMove('O',X,H):- machine('O','X',X,X2,H),
		  show(X2),
		  nextMove('X',X2,H).

%play(X,P,T,T2) is satisfied if T2 is the board T after player X moves in column P
play(X,P,board(T),board(T2)):- append(I,[C|F],T),
			       length(I,P),
                               playColumn(X,C,C2),
			       append(I,[C2|F],T2).

%playColumn(X,C,C2) is satisfied if column C2 is column C after player X plays there
playColumn(X,['-'],[X]):- !. % last spot in column
playColumn(X,['-',A|AS],[X,A|AS]):- A \== ('-'), !. % play above someone's piece
playColumn(X,['-'|AS],['-'|AS2]):- playColumn(X,AS,AS2). % descend column

%wins(X,T) is satisfied if player X has won in board T
%check if there's a column in T with 4 connected pieces of player X
wins(X,board(T)):- append(_, [C|_], T), % check if there's a column...
	           append(_,[X,X,X,X|_],C). % ...which has 4 connected pieces of player X
%check if there's a row in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M), length(I2,M), length(I3,M), length(I4,M). %...and every piece is in the same height
%check if there's a diagonal (type \) in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1. %...and every piece is within the same diagonal \
%check if there's a diagonal (type /) in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1. %...and every piece is within the same diagonal /

%full(T) is satisfied if there isn't any free spot ('-')
full(board(T)):- \+ (append(_,[C|_],T),
		 append(_,['-'|_],C)).

%%%%%%%%%%%%%%%%%%
%%% READ MOVES %%%
%%%%%%%%%%%%%%%%%%
%reads a column
readColumn(C):- nl, write('Column: '),
		repeat,
		get_char(L),
		associateColumn(L,C),
		col(C), !.

%associateColumn(L,C) column C is the column associated with char L
associateColumn(L,C):- atom_codes(L,[La|_]),
		       C is La - 65.

%associateChar(L, C) char L is the char associated with column C
associateChar(L, C):- Ln is 65+C,
		      atom_codes(L,[Ln]).

%valid columns
col(0).
col(1).
col(2).
col(3).
col(4).
col(5).
col(6).

%%%%%%%%%%%%%%%%%%
%%%%% MACHINE %%%%
%%%%%%%%%%%%%%%%%%
%machine(R,O,T,T2) Let R be the machine piece, O the opponent's piece and T the board game. Then T2 is board T after the machine movement
% win if possible
/*
machine(R,_,T,T2):- iMachine(R,T,C,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L),
		    nl,!.

% otherwise, if machine can't win within a move, play a move that doesn't allow opponent O to win and that would allow us to obtain a connected 4
machine(R,O,T,T2):- findall((Col,TA), (col(Col), play(R,Col,T,TA),\+ iMachine(O,TA,_,_), goodMove(R,Col,T)), [(C,T2)|_]),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L),
		    nl,!.
% otherwise play a move that doesn't allow opponent O to win
machine(R,O,T,T2):- findall((Col,TA), (col(Col), play(R,Col,T,TA),\+ iMachine(O,TA,_,_)), [(C,T2)|_]),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl,
		    write('-'),!.
% otherwise play a move intercepting one of the future winning options of opponent O
machine(R,O,T,T2):- iMachine(O,T,C,_),
		    play(R,C,T,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L) nl.

%We consider that a good move is one allowing us to win in a column. Further improvements: rows and diagonals.
goodMove(R,Col,board(T)):- append(I,[C|_],T),
			   length(I,Col),
			   maxConnected(R,C,MaxConn),
			   MaxConn >= 4.

*/


% -------------------------- Added code ----------------------------


% Play according to heuristic 1 play count all 4-in-a-row available
machine(R,_,T,T2,1):- alpha_beta(R,2,T,-200,200,C,_Value,1),
		    play(R,C,T,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl.

% 2 play count all 2 and 3-in-a-row currently on the board
machine(R,_,T,T2,2):- alpha_beta(R,2,T,-200,200,C,_Value,2),
		    play(R,C,T,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl.

% 3 play defensively
machine(R,_,T,T2,3):- heuristic_defensive('O',board(T),C),
		    play(R,C,T,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl.

% 4 play aggressively. NB: does not work correctly
machine(R,_,T,T2,4):- heuristic_aggressive('O',board(T),C),
		    play(R,C,T,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl.

% else play randomly
machine(R,_,T,T2,_):- heuristic_random(C),
		    play(R,C,T,T2),
		    nl, write('machine: '),
		    associateChar(L,C),
		    write(L), nl.

%iMachine(R,T,C,T2) is satisfied if player R can play in column C of board T and obtain a winning board T2
iMachine(R,T,C,T2):- findall((Col,TA), (col(Col), play(R,Col,T,TA),wins(R,TA)),[(C,T2)|_]).

/*
 * No longer in use
 *
% maxConnected(R,C,MaxConn) MaxConn is the maximum number of connected pieces that player R has/could have in column C
maxConnected(_,[],0).
maxConnected(R,[X|_],0):- X\=R.
maxConnected(R,['-'|X],N):- maxConnected(R,X,Ns),
			    N is Ns+1.
maxConnected(R,[R|X],N):- maxConnected(R,X,Ns),
			  N is Ns+1.

% valuePos evaluates the number of 4-in-a-rows that the board T
% currently contains

colRow(Pos, Col, Row) :- Col is Pos mod 7, Row is Pos div 7.

diags(Col,N) :- Col<4 -> N is 1; N is 2.

valuePos(Pos,E) :- colRow(Pos, Col, Row), diags(Col,N),
   E is min(Col,8-Col)+min(Row,7-Row)+1+N.

*/

%Heuristic AlphaBeta evaluating number of possible 4-in-a-row

% open(X,T) is satisfied if player X has a 4-in-a-row line open in board
% T, check if there's a column in T with 4 connected pieces of player X
% or empty
open(board(T),X):- append(_, [C|_], T), % check if there's a column...
	           append(_,[P1,P2,P3,P4|_],C), % ...which has 4 connected pieces..
                   (P1 == '-' | P1 == X), (P2 == '-' | P2 == X), %...of X or empty space
                   (P3 == '-' | P3 == X), (P4 == '-' | P4 == X).
%check if there's a row in T with 4 connected pieces of player X
open(board(T),X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C4),
		   length(I1,M), length(I2,M), length(I3,M), length(I4,M), %...and every piece is in the same height
                   (P1 == '-' | P1 == X), (P2 == '-' | P2 == X), %...of X or empty space
                   (P3 == '-' | P3 == X), (P4 == '-' | P4 == X).

%check if there's a diagonal (type \) in T with 4 connected pieces of player X
open(board(T),X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1, %...and every piece is within the same diagonal \
                   (P1 == '-' | P1 == X), (P2 == '-' | P2 == X), %...of X or empty space
                   (P3 == '-' | P3 == X), (P4 == '-' | P4 == X).

%check if there's a diagonal (type /) in T with 4 connected pieces of player X
open(board(T),X):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1, %...and every piece is within the same diagonal /
                   (P1 == '-' | P1 == X), (P2 == '-' | P2 == X), %...of X or empty space
                   (P3 == '-' | P3 == X), (P4 == '-' | P4 == X).

% twos(X,T) is satisfied if player X has any two in a row in board T
twos(board(T),X):- append(_, [C|_], T), % check if there's a column...
	           append(_,[P1,P2|_],C), % ...which has 2 connected pieces..
                   (P1 == X), (P2 == X). % ...of player X
%check if there's a row in T with 2 connected pieces of player X
twos(board(T),X):- append(_,[C1,C2|_],T), % check if 2 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   length(I1,M), length(I2,M), %...and every piece is in the same height
                   P1 == X, P2 == X. %...of X

% check if there's a diagonal (type \) in T with 2 connected pieces of
% player X
twos(board(T),X):- append(_,[C1,C2|_],T), % check if 2 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   length(I1,M1), length(I2,M2),
		   M2 is M1+1, %...and every piece is within the same diagonal \
                   P1 == X, P2 == X. %...of X

% check if there's a diagonal (type /) in T with 2 connected pieces of
% player X
twos(board(T),X):- append(_,[C1,C2|_],T), % check if 2 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   length(I1,M1), length(I2,M2),
		   M2 is M1-1, %...and every piece is within the same diagonal /
                   P1 == X, P2 == X. %...of X

% threes(X,T) is satisfied if player X has any three in a row in board T
threes(board(T),X):- append(_, [C|_], T), % check if there's a column...
	           append(_,[P1,P2,P3|_],C), % ...which has 3 connected pieces..
                   P1 == X, P2 == X, P3 == X. % ...of player X
%check if there's a row in T with 2 connected pieces of player X
threes(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M), length(I2,M), length(I3,M), %...and every piece is in the same height
                   P1 == X, P2 == X, P3 == X. %...of X

% check if there's a diagonal (type \) in T with 3 connected pieces of
% player X
threes(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M1), length(I2,M2), length(I3,M3),
		   M2 is M1+1, M3 is M2+1, %...and every piece is within the same diagonal \
                   P1 == X, P2 == X, P3 == X. %...of X

% check if there's a diagonal (type /) in T with 3 connected pieces of
% player X
threes(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M1), length(I2,M2), length(I3,M3),
		   M2 is M1-1, M3 is M2-1, %...and every piece is within the same diagonal /
                   P1 == X, P2 == X, P3 == X. %...of X

% twoBlocked(X,T) is satisfied if player X has any two-in-a-row that is
% blocked at one side in board T
twoBlocked(board(T),X):- append(_, [C|_], T), % check if there's a column...
	           append(_,[P1,P2,P3|_],C), % ...which has 3 connected pieces..
                   other_player(X,O),
                   ((P1 == X, P2 == X, P3 == O); % ...of player X,
                   (P1 == O, P2 == X, P3 == X)). % ...and one is other player

%check if there's a row in T with 2 connected pieces of player X
twoBlocked(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M), length(I2,M), length(I3,M), %...and every piece is in the same height
                   other_player(X,O),
                   ((P1 == X, P2 == X, P3 == O); % ...of player X,
                   (P1 == O, P2 == X, P3 == X)). % ...and one is other player

% check if there's a diagonal (type \) in T with 3 connected pieces of
% player X
twoBlocked(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M1), length(I2,M2), length(I3,M3),
		   M2 is M1+1, M3 is M2+1, %...and every piece is within the same diagonal \
                   other_player(X,O),
                   ((P1 == X, P2 == X, P3 == O); % ...of player X,
                   (P1 == O, P2 == X, P3 == X)). % ...and one is other player

% check if there's a diagonal (type /) in T with 3 connected pieces of
% player X
twoBlocked(board(T),X):- append(_,[C1,C2,C3|_],T), % check if 3 connected columns exists in board...
		   append(I1,[P1|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
                   length(I1,M1), length(I2,M2), length(I3,M3),
		   M2 is M1-1, M3 is M2-1, %...and every piece is within the same diagonal /
                   other_player(X,O),
                   ((P1 == X, P2 == X, P3 == O); % ...of player X,
                   (P1 == O, P2 == X, P3 == X)). % ...and one is other player

% possibleMoves (+T, -M) returns all possible each possible move M given
% the board T (M ranges from 0 to 6, if a column is full, the
% corresponding number will not be in the list)

possibleMoves(board(T), 0) :- append([],[C|_],T),
   length(C,6),
   member('-',C).
possibleMoves(board(T), 1) :- append(C2,[C|_],T),
   length(C2,1),
   length(C,6),
   member('-',C).
possibleMoves(board(T), 2) :- append(C2,[C|_],T),
   length(C2,2),
   length(C,6),
   member('-',C).
possibleMoves(board(T), 3) :- append(C2,[C|_],T),
   length(C2,3),
   length(C,6),
   member('-',C).
possibleMoves(board(T), 4) :- append(C2,[C|_],T),
   length(C2,4),
   length(C,6),
   member('-',C).
possibleMoves(board(T), 5) :- append(C2,[C|_],T),
   length(C2,5),
   length(C,6),
   member('-',C).
possibleMoves(board(T), 6) :- append(C2,[C|_],T),
   length(C2,6),
   length(C,6),
   member('-',C).

% valueX(T,E) returns the assessed "value" E of the board T for the
% player playing 'O'

% this heuristic calculates how many 4-pieces space are available to
% each player using open/2

valueOpen(T,100) :- wins('O',T), !.
valueOpen(T,-100) :- wins('X',T), !.
valueOpen(T,E) :-
   findall('O',open(T,'O'),MAX),
   length(MAX,Emax),      % # lines open to o
   findall('X',open(T,'X'),MIN),
   length(MIN,Emin),      % # lines open to x
   E is Emax - Emin.


% this heuristic calculates how many 2 pieces in a row are on the table
% for each player (3 pieces in a row would count as twice as much) using
% twos/2 and threes/2

valueCountTwosThrees(T,100) :- wins('O',T), !.
valueCountTwosThrees(T,-100) :- wins('X',T), !.
valueCountTwosThrees(T,E) :-
   findall('O',twos(T,'O'),MAX2),
   length(MAX2,Emax2),      % # of twos in a row for O
   findall('O',threes(T,'O'),MAX3),
   length(MAX3,Emax3),      % # of threes in a row for O
   findall('X',twos(T,'X'),MIN2),
   length(MIN2,Emin2),      % # of twos in a row for X
   findall('X',threes(T,'O'),MIN3),
   length(MIN3,Emin3),      % # of threes in a row for X
   valueBlocked(T,EB),
   E is (Emax2 - Emin2) + (Emax3 - Emin3)*0.5 - EB*1.1.
% E2 is number of 2-in-a-row, E3 is number of 3-in-a-row, and EB is
% number of 2-in-a-row blocked. Note: a 3-in-a-row counts also as 2
% 2-in-a-row (meaning a 3 in a row gives 2*E2 + E3 point).
% The weight of each term can and should be modified to find the optimal
% porportions. EB should be higher than E2 so that the machine will
% block a free 2-in-a-row instead of attempting to make a 2-in-a-row. E3
% should not be too high because of how a 3-in-a-row is calculated.

% valueBlocked(T,E) returns how many 2-in-a-row of player 'O' are
% blocked by 'X' minus how many of player 'X' are blocked by 'O'
valueBlocked(T,E) :-
   findall('O',twoBlocked(T,'O'),MAXB),
   length(MAXB,EmaxB),
   findall('X',twoBlocked(T,'X'),MINB),
   length(MINB,EminB),
   E is EmaxB-EminB.

/*
% No longer used
getRow(['-'],Row,RowN):- !, RowN is Row-1. % last spot in column
getRow(['-',A|_],Row,RowN):- A \== ('-'), !, RowN is Row-1. % play above someone's piece
getRow(['-'|AS],Row,RowN):- I is Row-1, getRow(AS,I,RowN). % descend column

translatePos(C,board(T),Pos) :- append(I,[Col|_],T),
                         length(I,C),
                         getRow(Col,6,RowN),
                         Pos is RowN*7+C+1.
*/

alpha_beta(_,0,T,_Alpha,_Beta,_NoMove,Value,1) :-
   valueOpen(T,Value).

alpha_beta(_,0,T,_Alpha,_Beta,_NoMove,Value,2) :-
   valueCountTwosThrees(T,Value).

alpha_beta(Player,D,T,Alpha,Beta,Move,Value,H) :-
   D > 0,
   findall(Col,possibleMoves(T,Col),Moves),
   Alpha1 is -Beta, % max/min
   Beta1 is -Alpha,
   D1 is D-1,
   evaluate_and_choose(Player,Moves,T,D1,Alpha1,Beta1,nil,(Move,Value),H).

evaluate_and_choose(Player,[Move|Moves],T,D,Alpha,Beta,Record,BestMove,H) :-
   play(Player,Move,T,TA),
   other_player(Player,OtherPlayer),
   alpha_beta(OtherPlayer,D,TA,Alpha,Beta,_OtherMove,Value,H),
   Value1 is -Value,
   cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,T,Record,BestMove,H).
evaluate_and_choose(_Player,[],_T,_D,Alpha,_Beta,Move,(Move,Alpha),_H).

cutoff(_Player,Move,Value,_D,_Alpha,Beta,_Moves,_T,_Record,(Move,Value),_H) :-
   Value >= Beta, !.
cutoff(Player,Move,Value,D,Alpha,Beta,Moves,T,_Record,BestMove,H) :-
   Alpha < Value, Value < Beta, !,
   evaluate_and_choose(Player,Moves,T,D,Value,Beta,Move,BestMove,H).
cutoff(Player,_Move,Value,D,Alpha,Beta,Moves,T,Record,BestMove,H) :-
   Value =< Alpha, !,
   evaluate_and_choose(Player,Moves,T,D,Alpha,Beta,Record,BestMove,H).

other_player('O','X').
other_player('X','O').

testBoard(board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','X'],
	       ['-','-','-','O','X','X'],
	       ['-','-','-','-','-','O'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','O']])).


% Heuristic Random which finds a column randomly
% To use this heuristic, use heuristic_random(C), where C is the column chosen

heuristic_random(C) :-
   random_between(1,7,C).

% Heuristic Defensive which finds a column to stop the opposant
% To use this heuristic, use heuristic_defensive(Player,board(T),C), where Player is the player in round, T the actual board, and C the column chosen by heuristic

% To win if possible in 1 step
heuristic_defensive(Player,T,C) :-
   iMachine(Player,T,C,_).

% otherwise, stop the opposant of 3-in-row
heuristic_defensive(Player,board(T),C) :-
   other_player(Player,Other),
   findall(Col,defensive(T,Other,Col),[C|_]).
%   defensive(T,Other,Col),
%   findindex(C,T,Col).

% otherwise, place randomly
heuristic_defensive(_,_,C) :-
   heuristic_random(C).

% defensive(T,X,C) means in board T, for player X, there exists 2-in-row. It's possible to stop it by placing in column C

% 3-in-row
% find 3 in Column while not reaching the top
defensive(board(T),X,C):- TT=T,
	           append(_, [Cc|_], T),
	           append(_,['-',P1,P2,P3|_],Cc),
                   P1 == X, P2 == X, P3 == X,
		   nth0(C,TT,Cc).

% find 3 in row while the right point is reachable
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,C3,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M), length(I2,M), length(I3,M),length(I4,M),length(I4X,M1),
		   M1 is M+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col3+1, Col3 is Col2+1, Col2 is Col1+1.

% find 3 in row while the left point is reachable
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M), length(I2,M), length(I3,M),length(I4,M),length(I4X,M1),
		   M1 is M+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1, Col3 is Col2+1, Col2 is Col1+1.


% find 3 in row while the right point is reachable (case bottom)
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,C3,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
                   length(I1,M), length(I2,M), length(I3,M),length(I4,M),
		   M ==5,
                   P1 == X, P2 == X, P3 == X,P4 == '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col3+1, Col3 is Col2+1, Col2 is Col1+1.

% find 3 in row while the left point is reachable (case bottom)
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
                   length(I1,M), length(I2,M), length(I3,M),length(I4,M),
		   M ==5,
                   P1 == X, P2 == X, P3 == X,P4 == '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1, Col3 is Col2+1, Col2 is Col1+1.

% find 3 in diagonal \ while the right point is reachable
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,C3,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),length(I4X,M4X),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1, M4X is M4+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col3+1, Col3 is Col2+1, Col2 is Col1+1.

% find 3 in diagonal \ while the right point is reachable (case bottom)
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,C3,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1,M4==5,
                   P1 == X, P2 == X, P3 == X,P4 == '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col3+1, Col3 is Col2+1, Col2 is Col1+1.

% find 3 in diagonal \ while the left point is reachable
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),length(I4X,M4X),
		   M1 is M4+1, M2 is M1+1, M3 is M2+1,  M4X is M4+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1, Col3 is Col2+1, Col2 is Col1+1.

% find 3 in diagonal / while the right point is reachable
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,C3,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),length(I4X,M4X),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1, M4X is M4+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col3+1, Col3 is Col2+1, Col2 is Col1+1.

% find 3 in diagonal / while the left point is reachable
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),length(I4X,M4X),
		   M1 is M4-1,M2 is M1-1, M3 is M2-1,  M4X is M4+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1, Col3 is Col2+1, Col2 is Col1+1.

% find 3 in diagonal / while the left point is reachable (case bottom)
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],Cc),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),
		   M1 is M4-1,M2 is M1-1, M3 is M2-1, M4==5,
                   P1 == X, P2 == X, P3 == X,P4 == '-',
		   nth0(C,TT,Cc),nth0(Col3,TT,C3),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1, Col3 is Col2+1, Col2 is Col1+1.

% 2-in-row
% find 2 in Column while not reaching the top
defensive(board(T),X,C):- TT=T,append(_, [Cc|_], T),
	           append(_,[P,P1,P2|_],Cc),
                   (P1 == X), (P2 == X), (P == '-'),
		   nth0(C,TT,Cc).

% find 2 in row while the right point is reachable
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],Cc),
		   append(I3X,[P3X|_],Cc),
                   length(I1,M), length(I2,M),length(I3,M),length(I3X,M1),
		   M1 is M+1,
                   P1 == X, P2 == X, P3 == '-',P3X \== '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col2+1, Col2 is Col1+1.

% find 2 in row while the left point is reachable
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],Cc),
		   append(I3X,[P3X|_],Cc),
                   length(I1,M), length(I2,M),length(I3,M),length(I3X,M1),
		   M1 is M+1,
                   P1 == X, P2 == X, P3 == '-',P3X \== '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1,Col2 is Col1+1.

% find 2 in row while the right point is reachable (case bottom)
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],Cc),
                   length(I1,M), length(I2,M),length(I3,M),
		   M==5,
                   P1 == X, P2 == X, P3 == '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col2+1, Col2 is Col1+1.

% find 2 in row while the left point is reachable (case bottom)
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],Cc),
                   length(I1,M), length(I2,M),length(I3,M),
		   M==5,
                   P1 == X, P2 == X, P3 == '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1,Col2 is Col1+1.

% find 2 in diagonal \ while the right point is reachable
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M1), length(I2,M2), length(I4,M4),length(I4X,M4X),
		   M2 is M1+1, M4 is M2+1, M4X is M4+1,
                   P1 == X, P2 == X, P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col2+1, Col2 is Col1+1.

% find 2 in diagonal \ while the right point is reachable (case bottom)
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],Cc),
                   length(I1,M1), length(I2,M2), length(I4,M4),
		   M2 is M1+1, M4 is M2+1,M4==5,
                   P1 == X, P2 == X, P4 == '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col2+1, Col2 is Col1+1.

% find 2 in diagonal \ while the left point is reachable
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M1), length(I2,M2), length(I4,M4),length(I4X,M4X),
		   M1 is M4+1, M2 is M1+1,  M4X is M4+1,
                   P1 == X, P2 == X,P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1,Col2 is Col1+1.

% find 2 in diagonal / while the right point is reachable
defensive(board(T),X,C):- TT=T,append(_,[C1,C2,Cc|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M1), length(I2,M2),length(I4,M4),length(I4X,M4X),
		   M2 is M1-1, M4 is M2-1,  M4X is M4+1,
                   P1 == X, P2 == X, P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col2+1, Col2 is Col1+1.

% find 2 in diagonal / while the left point is reachable
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],Cc),
		   append(I4X,[P4X|_],Cc),
                   length(I1,M1), length(I2,M2), length(I4,M4),length(I4X,M4X),
		   M1 is M4-1,M2 is M1-1, M4X is M4+1,
                   P1 == X, P2 == X, P4 == '-',P4X \== '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1,Col2 is Col1+1.

% find 2 in diagonal / while the left point is reachable (case bottom)
defensive(board(T),X,C):- TT=T,append(_,[Cc,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],Cc),
                   length(I1,M1), length(I2,M2), length(I4,M4),
		   M1 is M4-1,M2 is M1-1, M4==5,
                   P1 == X, P2 == X, P4 == '-',
		   nth0(C,TT,Cc),nth0(Col2,TT,C2),nth0(Col1,TT,C1),
		   C is Col1-1,Col2 is Col1+1.

% Heuristic Aggressive which finds a column to help us win

% search if we have a 4-in-row
heuristic_aggressive(Player,T,C) :- iMachine(Player,T,C,_).

% search if opponent can form a 4-in-row within one move.
heuristic_aggressive(Player,T,C) :- other_player(Player,Other),iMachine(Other,T,C,_).

% if not, try to form a 3-in-row
heuristic_aggressive(Player,T,C) :-
   TT=T,
   findall(Col,aggressive(T,Player,Col),[Cc|_]),
   nth0(C,TT,Cc).


% aggressive(T,X,C) means in board T, for player X, there exists 2-in-row and it is possible to form more by placing in column C

% 3-in-row
% find 3 in Column while not reaching the top
aggressive(T,X,C):- append(_, [C|_], T),
	           append(_,['-',P1,P2,P3|_],C),
                   P1 == X, P2 == X, P3 == X.

% find 3 in row while the right point is reachable
aggressive(T,X,C):- append(_,[C1,C2,C3,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M), length(I2,M), length(I3,M),length(I4,M),length(I4X,M1),
		   M1 is M+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-'.

% find 3 in row while the left point is reachable
aggressive(T,X,C):- append(_,[C,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M), length(I2,M), length(I3,M),length(I4,M),length(I4X,M1),
		   M1 is M+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-'.

% find 3 in row while the right point is reachable (case bottom)
aggressive(T,X,C):- append(_,[C1,C2,C3,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
                   length(I1,M), length(I2,M), length(I3,M),length(I4,M),
		   M ==5,
                   P1 == X, P2 == X, P3 == X,P4 == '-'.

% find 3 in row while the left point is reachable (case bottom)
aggressive(T,X,C):- append(_,[C,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
                   length(I1,M), length(I2,M), length(I3,M),length(I4,M),
		   M ==5,
                   P1 == X, P2 == X, P3 == X,P4 == '-'.

% find 3 in diagonal \ while the right point is reachable
aggressive(T,X,C):- append(_,[C1,C2,C3,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),length(I4X,M4X),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1, M4X is M4+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-'.

% find 3 in diagonal \ while the right point is reachable (case bottom)
aggressive(T,X,C):- append(_,[C1,C2,C3,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1,M4==5,
                   P1 == X, P2 == X, P3 == X,P4 == '-'.

% find 3 in diagonal \ while the left point is reachable
aggressive(T,X,C):- append(_,[C,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),length(I4X,M4X),
		   M1 is M4+1, M2 is M1+1, M3 is M2+1,  M4X is M4+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-'.

% find 3 in diagonal / while the right point is reachable
aggressive(T,X,C):- append(_,[C1,C2,C3,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),length(I4X,M4X),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1, M4X is M4+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-'.

% find 3 in diagonal / while the left point is reachable
aggressive(T,X,C):- append(_,[C,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),length(I4X,M4X),
		   M1 is M4-1,M2 is M1-1, M3 is M2-1,  M4X is M4+1,
                   P1 == X, P2 == X, P3 == X,P4 == '-',P4X \== '-'.

% find 3 in diagonal / while the left point is reachable (case bottom)
aggressive(T,X,C):- append(_,[C,C1,C2,C3|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
                   append(I3,[P3|_],C3),
		   append(I4,[P4|_],C),
                   length(I1,M1), length(I2,M2), length(I3,M3),length(I4,M4),
		   M1 is M4-1,M2 is M1-1, M3 is M2-1, M4==5,
                   P1 == X, P2 == X, P3 == X,P4 == '-'.

% 2-in-row
% find 2 in Column while not reaching the top
aggressive(T,X,C):- append(_, [C|_], T),
	           append(_,[P,P1,P2|_],C),
                   (P1 == X), (P2 == X), (P == '-').

% find 2 in row while the right point is reachable
aggressive(T,X,C):- append(_,[C1,C2,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C),
		   append(I3X,[P3X|_],C),
                   length(I1,M), length(I2,M),length(I3,M),length(I3X,M1),
		   M1 is M+1,
                   P1 == X, P2 == X, P3 == '-',P3X \== '-'.

% find 2 in row while the left point is reachable
aggressive(T,X,C):- append(_,[C,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C),
		   append(I3X,[P3X|_],C),
                   length(I1,M), length(I2,M),length(I3,M),length(I3X,M1),
		   M1 is M+1,
                   P1 == X, P2 == X, P3 == '-',P3X \== '-'.

% find 2 in row while the right point is reachable (case bottom)
aggressive(T,X,C):- append(_,[C1,C2,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C),
                   length(I1,M), length(I2,M),length(I3,M),
		   M==5,
                   P1 == X, P2 == X, P3 == '-'.

% find 2 in row while the left point is reachable (case bottom)
aggressive(T,X,C):- append(_,[C,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I3,[P3|_],C),
                   length(I1,M), length(I2,M),length(I3,M),
		   M==5,
                   P1 == X, P2 == X, P3 == '-'.

% find 2 in diagonal \ while the right point is reachable
aggressive(T,X,C):- append(_,[C1,C2,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M1), length(I2,M2), length(I4,M4),length(I4X,M4X),
		   M2 is M1+1, M4 is M2+1, M4X is M4+1,
                   P1 == X, P2 == X, P4 == '-',P4X \== '-'.

% find 2 in diagonal \ while the right point is reachable (case bottom)
aggressive(T,X,C):- append(_,[C1,C2,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],C),
                   length(I1,M1), length(I2,M2), length(I4,M4),
		   M2 is M1+1, M4 is M2+1,M4==5,
                   P1 == X, P2 == X, P4 == '-'.

% find 2 in diagonal \ while the left point is reachable
aggressive(T,X,C):- append(_,[C,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M1), length(I2,M2), length(I4,M4),length(I4X,M4X),
		   M1 is M4+1, M2 is M1+1,  M4X is M4+1,
                   P1 == X, P2 == X,P4 == '-',P4X \== '-'.

% find 2 in diagonal / while the right point is reachable
aggressive(T,X,C):- append(_,[C1,C2,C|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M1), length(I2,M2),length(I4,M4),length(I4X,M4X),
		   M2 is M1-1, M4 is M2-1,  M4X is M4+1,
                   P1 == X, P2 == X, P4 == '-',P4X \== '-'.

% find 2 in diagonal / while the left point is reachable
aggressive(T,X,C):- append(_,[C,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],C),
		   append(I4X,[P4X|_],C),
                   length(I1,M1), length(I2,M2), length(I4,M4),length(I4X,M4X),
		   M1 is M4-1,M2 is M1-1, M4X is M4+1,
                   P1 == X, P2 == X, P4 == '-',P4X \== '-'.

% find 2 in diagonal / while the left point is reachable (case bottom)
aggressive(T,X,C):- append(_,[C,C1,C2|_],T),
		   append(I1,[P1|_],C1),
		   append(I2,[P2|_],C2),
		   append(I4,[P4|_],C),
                   length(I1,M1), length(I2,M2), length(I4,M4),
		   M1 is M4-1,M2 is M1-1, M4==5,
                   P1 == X, P2 == X, P4 == '-'.

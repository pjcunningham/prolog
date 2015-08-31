
:- dynamic board/1.
:- retractall(board(_)).
:- assert(board([_Z1,_Z2,_Z3,_Z4,_Z5,_Z6,_Z7,_Z8,_Z9])). 


%%%%%
%%  Generate possible marks on a free spot on the board.
%%  Use mark(+,+,-Row,-Column) to query/generate possible moves (X,Y).
%%%%%
mark(Player, [X|_],1,1) :- var(X), X=Player.
mark(Player, [_,X|_],2,1) :- var(X), X=Player.
mark(Player, [_,_,X|_],3,1) :- var(X), X=Player.
mark(Player, [_,_,_,X|_],1,2) :- var(X), X=Player.
mark(Player, [_,_,_,_,X|_],2,2) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,X|_],3,2) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,X|_],1,3) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,_,X|_],2,3) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,_,_,X|_],3,3) :- var(X), X=Player.

%%  Record a move: record(+,+,+).

record(Player, X, Y) :- 
   retract(board(B)), 
   mark(Player,B,X,Y),
   assert(board(B)).

draw :-
	board([A, B, C, D, E, F, G, H, I]),
	write('\n'),
	draw_cell(A), write('|'), draw_cell(B), write('|'), draw_cell(C), write('\n'), 
	draw_cell(D), write('|'), draw_cell(E), write('|'), draw_cell(F), write('\n'),
	draw_cell(G), write('|'), draw_cell(H), write('|'), draw_cell(I), write('\n'), write('\n').
	
draw_cell(X) :-
	var(X), write('-').
	
draw_cell(X) :-
	nonvar(X), write(X).

show_moves :-
 board(B), findall((X,Y), mark(o,B,X,Y), Moves).
 
%%%%%
%% Calculate the value of a position, o maximizes, x minimizes.
%%%%%
value(Board,100) :- win(Board,o), !.
value(Board,-100) :- win(Board,x), !.
value(Board,E) :- 
   findall(o,open(Board,o),MAX), 
   length(MAX,Emax),      % # lines open to o
   findall(x,open(Board,x),MIN), 
   length(MIN,Emin),      % # lines open to x
   E is Emax - Emin.
 
%%%%% 
%%  A winning line is ALREADY bound to Player. 
%%  win(+Board,+Player) is true or fail.
%%    e.g., win([P,P,P|_],P).  is NOT correct, because could bind 
%%%%%
win([Z1,Z2,Z3|_],P) :- Z1==P, Z2==P, Z3==P.
win([_,_,_,Z1,Z2,Z3|_],P) :-  Z1==P, Z2==P, Z3==P.
win([_,_,_,_,_,_,Z1,Z2,Z3],P) :-  Z1==P, Z2==P, Z3==P.
win([Z1,_,_,Z2,_,_,Z3,_,_],P) :-  Z1==P, Z2==P, Z3==P.
win([_,Z1,_,_,Z2,_,_,Z3,_],P) :-  Z1==P, Z2==P, Z3==P.
win([_,_,Z1,_,_,Z2,_,_,Z3],P) :-  Z1==P, Z2==P, Z3==P.
win([Z1,_,_,_,Z2,_,_,_,Z3],P) :-  Z1==P, Z2==P, Z3==P.
win([_,_,Z1,_,Z2,_,Z3,_,_],P) :-  Z1==P, Z2==P, Z3==P.

%%%%%
%%  A line is open if each position is either free or equals the Player
%%%%%
open([Z1,Z2,Z3|_],Player) :-
	(var(Z1) ; Z1 == Player), (var(Z2) ; Z2 == Player), (var(Z3) ; Z3 == Player).
	
open([_,_,_,Z1,Z2,Z3;_],Player) :- 
	(var(Z1) ; Z1 == Player),(var(Z2) ; Z2 == Player), (var(Z3) ; Z3 == Player).
	
open([_,_,_,_,_,_,Z1,Z2,Z3],Player) :-
	(var(Z1) ; Z1 == Player),(var(Z2) ; Z2 == Player), (var(Z3) ; Z3 == Player).
	
open([Z1,_,_,Z2,_,_,Z3,_,_],Player) :-
	(var(Z1) ; Z1 == Player),(var(Z2) ; Z2 == Player), (var(Z3) ; Z3 == Player).
	
open([_,Z1,_,_,Z2,_,_,Z3,_],Player) :-
	(var(Z1) ; Z1 == Player),(var(Z2) ; Z2 == Player), (var(Z3) ; Z3 == Player).
	
open([_,_,Z1,_,_,Z2,_,_,Z3],Player) :- 
	(var(Z1) ; Z1 == Player),(var(Z2) ; Z2 == Player), (var(Z3) ; Z3 == Player).
	
open([Z1,_,_,_,Z2,_,_,_,Z3],Player) :- 
	(var(Z1) ; Z1 == Player),(var(Z2) ; Z2 == Player), (var(Z3) ; Z3 == Player).
		
open([_,_,Z1,_,Z2,_,Z3,_,_],Player) :- 
	(var(Z1) ; Z1 == Player),(var(Z2) ; Z2 == Player), (var(Z3) ; Z3 == Player).


alpha_beta(Player,0,Position,_Alpha,_Beta,_NoMove,Value) :- 
   value(Position,Value).

alpha_beta(Player,D,Position,Alpha,Beta,Move,Value) :- 
   D > 0, 
   findall((X,Y),mark(Player,Position,X,Y),Moves), 
   Alpha1 is -Beta, % max/min
   Beta1 is -Alpha,
   D1 is D-1, 
   evaluate_and_choose(Player,Moves,Position,D1,Alpha1,Beta1,nil,(Move,Value)).

evaluate_and_choose(Player,[Move|Moves],Position,D,Alpha,Beta,Record,BestMove) :-
   move(Player,Move,Position,Position1), 
   other_player(Player,OtherPlayer),
   alpha_beta(OtherPlayer,D,Position1,Alpha,Beta,_OtherMove,Value),
   Value1 is -Value,
   cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,Position,Record,BestMove).
evaluate_and_choose(_Player,[],_Position,_D,Alpha,_Beta,Move,(Move,Alpha)).

cutoff(_Player,Move,Value,_D,_Alpha,Beta,_Moves,_Position,_Record,(Move,Value)) :- 
   Value >= Beta, !.
cutoff(Player,Move,Value,D,Alpha,Beta,Moves,Position,_Record,BestMove) :- 
   Alpha < Value, Value < Beta, !, 
   evaluate_and_choose(Player,Moves,Position,D,Value,Beta,Move,BestMove).
cutoff(Player,_Move,Value,D,Alpha,Beta,Moves,Position,Record,BestMove) :- 
   Value =< Alpha, !, 
   evaluate_and_choose(Player,Moves,Position,D,Alpha,Beta,Record,BestMove).

other_player(o,x).
other_player(x,o).

h(X,Y) :- record(x, X, Y), draw.

c :- 
   board(B), 
   alpha_beta(o,2,B,-200,200,(X,Y),_Value), % <=== NOTE
   write(X), write(Y),
   record(o,X,Y), draw.

play :-
	board(B),
	draw,
	record(x, 1, 1),
	draw,
	record(o, 1, 2),
	draw,
	record(x, 1, 3),
	draw.
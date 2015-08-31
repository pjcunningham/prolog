:- import(list).

main :- 
	read(X),
	primes(X, Primes),
	write_list(Primes, `\n`).

primes(N, Primes) :-
	list_of_integers(2, N, Integers),
	sieve(Integers, Primes).
	

list_of_integers(Lo, Hi, [Lo|Rest]) :-
	Lo =< Hi,
	!,
	M is Lo + 1,
	list_of_integers(M, Hi, Rest).

list_of_integers(_, _, []).

sieve([], []).

sieve([H|IntegerList], [H|Primes]) :-
	remove_multiple_of(H, IntegerList, NewIntegerList),
	sieve(NewIntegerList, Primes).
	

remove_multiple_of(P, [], []).

remove_multiple_of(P, [H|T], [H|NewT]) :-
	not(0 is H mod P),
	!,
	remove_multiple_of(P, T, NewT).
	 
remove_multiple_of(P, [H|T], NewT) :-
	0 is H mod P,
	!,
	remove_multiple_of(P, T, NewT).

% P06 (*) Find out whether a list is a palindrome.
% A palindrome can be read forward or backward; e.g. [x,a,m,a,x].

ispalindrome(X) :-
	reverse(X, X).
		
reverse(L1,L2) :- my_rev(L1,L2,[]).

my_rev([],L2,L2) :- !.

my_rev([X|Xs],L2,Acc) :- my_rev(Xs,L2,[X|Acc]).	
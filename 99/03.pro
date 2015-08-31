% Find the K'th element of a list.
% The first element in the list is number 1.
% Example:
% ?- element_at(X,[a,b,c,d,e],3).
% X = c

element_at(X, [X|_], 1).
element_at(X, [_|T], K) :-
	K > 1,
	K1 is K - 1,
	element_at(X, T, K1).
% Find the last element of a list.
% Example:
% last(X,[a,b,c,d]).
% X = d

last(X, [X]).
last(X, [H|T]) :-
	last(X, T).
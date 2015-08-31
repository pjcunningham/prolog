% Find the last but one element of a list.

lastbutone(X, [X, _]).
lastbutone(X, [_|T]) :-
	lastbutone(X, T).
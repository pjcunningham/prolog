% P04 (*) Find the number of elements of a list.

count_list([], 0).

count_list([_|T], C) :-
	count_list(T, C1),
	C is C1 + 1.
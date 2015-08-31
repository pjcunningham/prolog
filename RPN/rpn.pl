%% stack ops

%% op(Name, Arity, Doc)

rpn_op('+', 2, 'replace top two values on the stack with their sum').
rpn_op('-', 2, 'replace top two values on the stack with their difference').
rpn_op('*', 2, 'replace top two values on the stack with their product').
rpn_op('/', 2, 'replace top two values on the stack with their quotient').
rpn_op('div', 2, 'replace top two values on the stack with their integer division').
rpn_op('^', 2, 'replace top two values on the stack, x and y, with x to the yth power').


rpn_op('inv', 1, 'replace top value on the stack with its inverse').
rpn_op('neg', 1, 'replace top value on the stack with its negation').

rpn_op('sin', 1, 'replace top value on the stack with its sin').
rpn_op('cos', 1, 'replace top value on the stack with its cos').
rpn_op('tan', 1, 'replace top value on the stack with its tan').

rpn_op('asin', 1, 'replace top value on the stack with its asin').
rpn_op('acos', 1, 'replace top value on the stack with its acos').
rpn_op('atan', 1, 'replace top value on the stack with its atan').

rpn_op('drop', 1, 'remove the top value from the stack').
rpn_op('dup', 1, 'duplicate the top value on the stack').
rpn_op('swap', 2, 'swap the top two values on the stack').
rpn_op('clear', 0, 'clears the stack').

rpn_op('pi', 0, 'add PI to the top of the stack').
rpn_op('e', 0, 'add e to the top of the stack').


rpn_op('undo', 0, 'show this help').
rpn_op('redo', 0, 'show this help').

rpn_op('cut', 1, 'show this help').
rpn_op('copy', 1, 'show this help').
rpn_op('paste', 0, 'show this help').

rpn_op('help', 0, 'show this help').

op_arity(O, N) :- rpn_op(O, N, _).
op_doc(O, D) :- rpn_op(O, _, D).
op_names(Names) :- findall(N, rpn_op(N, _, _), Names).

binary_op(Op, [Y:Y1,X:X1|Stack], [Z:S|Stack]) :-
	Q =.. [Op, X, Y],
	call(Z is Q),
	term_string(Q, S).

unary_op(Op, [X:X1|Stack], [Z:S|Stack]) :-
	Q =.. [Op, X],
	call(Z is Q),
	term_string(Q, S).


act('+', S1, S2) :- !, binary_op('+', S1, S2).
act('-', S1, S2) :- !, binary_op('-', S1, S2).
act('*', S1, S2) :- !, binary_op('*', S1, S2).
act('/', S1, S2) :- !, binary_op('/', S1, S2).
act('div', S1, S2) :- !, binary_op('div', S1, S2).
act('^', S1, S2) :- !, binary_op('**', S1, S2).

act('inv', S1, S2) :- !, binary_op('inv', S1, S2).

act('neg', S1, S2) :- !, unary_op('-', S1, S2).
act('sin', S1, S2) :- !, unary_op('sin', S1, S2).
act('cos', S1, S2) :- !, unary_op('cos', S1, S2).
act('tan', S1, S2) :- !, unary_op('tan', S1, S2).
act('asin', S1, S2) :- !, unary_op('asin', S1, S2).
act('acos', S1, S2) :- !, unary_op('acos', S1, S2).
act('atan', S1, S2) :- !, unary_op('atan', S1, S2).


act('pi', Stack, [Z:"PUSH pi"|Stack]) :- Z is pi, !.
act('e', Stack, [Z:"PUSH e"|Stack]) :- Z is e, !.

act('drop', [_|Stack], Stack) :- !.
act('dup', [X|Stack], [X,X|Stack]) :- !.
act('swap', [Y,X|Stack], [X,Y|Stack]) :- !.
act('clear', _, []) :- !.

act('undo', Stack, Stack) :- !.
act('redo', Stack, Stack) :- !.

act('cut', Stack, Stack) :- !.
act('copy', Stack, Stack) :- !.
act('paste', Stack, Stack) :- !.

act('help', X, X) :-
	op_names(O),
	length(O, N),
	write(N),
	writeln(' Commands:'),
	op_help(O).

%% deal with list-ified commands (from readln), empty commands, stack underflow,
%% numerical values to push to stack, and unknown commands
act([X], S1, S2) :- !, act(X, S1, S2).
act([], S, S).
act(Op, S, S) :- bad_arity(Op, S), !.
act(N, Stack, [N:S|Stack]) :- number(N), string_concat('PUSH ', N, S),!.
act(_, X, X) :- signal_error('unknown operation').

%% support for the help command
op_help([]).
op_help([O|Ops]) :-
	op_doc(O, H),
	write(O), write(' -- '), writeln(H),
	op_help(Ops).

list_stack([], _) :- !.

list_stack([Value:Operation|T], Counter) :-
	format('SP ~|~`0t~d~5+ => ~a [~a] ~n', [Counter, Value, Operation]),
	Counter1 is Counter + 1,
	list_stack(T, Counter1).

%% check arity, and signal error if so (and succeed, for use in rule above)
bad_arity(Op, S) :- op_arity(Op, N), length(S, L), N > L, signal_error('stack underflow').

signal_error(S) :- atom_concat('*** ERROR: ', S, T), writeln(T).

repl1(Stack, NewStack) :- readln(S), !, act(S, Stack, NewStack), list_stack(NewStack, 0), nl.
repl(_, _) :- at_end_of_stream, !, writeln(''), halt.
repl(Stack, NewStack) :- !, repl1(Stack, TmpStack), repl(TmpStack, NewStack).
repl :- prompt(_, '> '), repl([], _).

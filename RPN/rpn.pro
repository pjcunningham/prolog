%% stack ops

%% op(Name, Arity, Doc)
rpn_op('.', 1, 'display the top value on the stack').
rpn_op('#', 0, 'display the number of values on the stack').
rpn_op('list', 0, 'list values on the stack').

rpn_op('+', 2, 'replace top two values on the stack with their sum').
rpn_op('-', 2, 'replace top two values on the stack with their difference').
rpn_op('*', 2, 'replace top two values on the stack with their product').
rpn_op('/', 2, 'replace top two values on the stack with their quotient').
rpn_op('^', 2, 'replace top two values on the stack, x and y, with x to the yth power').

rpn_op('sin', 1, 'replace top value on the stack with it''s sin').
rpn_op('cos', 1, 'replace top value on the stack with it''s cos').
rpn_op('tan', 1, 'replace top value on the stack with it''s tan').



rpn_op('drop', 1, 'remove the top value from the stack').
rpn_op('dup', 1, 'duplicate the top value on the stack').
rpn_op('swap', 2, 'swap the top two values on the stack').
rpn_op('clear', 0, 'clears the stack').
rpn_op('quit', 0, 'halt the program').

%% X = 10, Y = 15, Q =..[-, X, Y], call(Z is Q).
 
%% X = 90, Y = 15, Op = sin, Q =..[Op, X], call(Z is Q).

rpn_op('help', 0, 'show this help').

op_arity(O, N) :- rpn_op(O, N, _).
op_doc(O, D) :- rpn_op(O, _, D).
op_names(Names) :- findall(N, rpn_op(N, _, _), Names).

binary_op(Op, [Y:Y1,X:X1|Stack], [Z:S|Stack]) :-
	Q =.. [Op, X, Y],
	call(Z is Q),
	atomics_to_string([X, Op, Y], ' ', S).

unary_op(Op, [X:X1|Stack], [Z:S|Stack]) :-
	Q =.. [Op, X],
	call(Z is Q),
	atomics_to_string([Op, X], ' ', S).


act('.', [X|Stack], [X|Stack]) :- !.
act('#', Stack, Stack) :- !, length(Stack, X).
act('+', S1, S2) :- !, binary_op('+', S1, S2).
act('-', S1, S2) :- !, binary_op('-', S1, S2).
act('*', S1, S2) :- !, binary_op('*', S1, S2).
act('/', S1, S2) :- !, binary_op('/', S1, S2).
act('^', S1, S2) :- !, binary_op('**', S1, S2).


%%act('sin', [X|Stack], [Z|Stack]) :- !, Z is sin(X).
act('sin', S1, S2) :- !, unary_op('sin', S1, S2).
act('cos', [X|Stack], [Z|Stack]) :- !, Z is cos(X).
act('tan', [X|Stack], [Z|Stack]) :- !, Z is tan(X).


act('drop', [_|Stack], Stack) :- !.
act('dup', [X|Stack], [X,X|Stack]) :- !.
act('swap', [Y,X|Stack], [X,Y|Stack]) :- !.
act('clear', _, []) :- !.
act('quit', _, _) :- halt, !.
act('list', Stack, Stack) :- !, list_stack(Stack, 0).

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
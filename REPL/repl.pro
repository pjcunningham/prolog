main :-
	catch(repl, Error, error_handler(Error)).


repl :-
	repeat,
	write(`Enter > `),
	read_string(Input),
	string_split(Input, ` `, List),
	catch( rpn_list(List), Error, handle_error(Error) ),
	fail.
	
	
handle_error(quit) :-
	write(`Quitting\n`),
	throw(quit).

handle_error(_) :-
	write(`Invalid RPN term, try again.\n`),
   fail.
   
rpn_list([]).   % empty list, end.
rpn_list([S|Tail]) :-
   write(S),   % write the head
   nl,
   string_atom(S, T),
   evaluate(T), 
   !,
   rpn_list(Tail).  % recurse with the tail

evaluate(end) :-
	throw(quit).
   
evaluate(T) :-
	number(T),
	write(`Is Number`:T), nl.
	
evaluate(T) :-
	write(`Is Something Else`:T), nl.
	
process_number(N).

process_Operator(Operator).

process_Command(Command).
	
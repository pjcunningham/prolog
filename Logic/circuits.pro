:-op(720, fy, ~).
:-op(730, yfx, &).
:-op(740, yfx, ^).


half_adder(A,B,Sum,Carry) -->
	sum_circuit(A,B,Sum),
	carry_circuit(A,B,Carry).

sum_circuit(A, B, Sum) --> comb_tree([A,B], Sum,(~ A & B) ^ (A & ~ B)).

carry_circuit(A, B, Carry) --> comb_tree([A, B], Carry, A & B).

comb_tree(Out,InS,Eq) -->
	component(Out,InNetS,Eq,ArgS), % Node with
	descendants(InNetS,InS,ArgS). % descendants.
	
descendants(NetS,InS,Eq) --> % Recursive
		{concat(Net,NetS1,NetS), concat(Eq1,EqR,Eq)},
		descendant(Net,InS1,Eq1), % decomposition
		descendants(NetS1,InS2,EqR), % of descendants or
		{add_to_set(InS1,InS2,InS)}.

descendants(NetS,InS,Eq) -->
	{concat(Net,[],NetS),
	concat(Eq1,[],Eq)},
	descendant(Net,InS,Eq1). % single descendant.

descendant(Out,InS,Eq) -->
	comb_tree(Out,InS,Eq). % tree or
	
descendant(Out,In,Out) --> % (note Out = Eq)
	{external(Out), % leaf
	concat(Out,[],In)}.

component(Out,[In],  ~ A, [A]) --> [invert(In,Out)].

component(Out, [In1, In2], (A ^ B), [A,B])--> [or(In1, In2, Out)].

component(Out, [In1, In2], (A & B), [A, B])--> [and(In1, In2, Out)].

concat([X|Y],Z,[X|W]) :- concat(Y,Z,W).
concat([],X,X).

invert(1, 0).
invert(0, 1).

and(0, 0, 0).
and(0, 1, 0).
and(1, 0, 0).
and(1, 1, 0).

or(0, 0, 0).
or(0, 1, 1).
or(1, 0, 1).
or(1, 1, 1).
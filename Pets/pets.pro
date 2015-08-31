pet(dog) :- lives_in(house), sound(woof).
pet(duck) :- legs(2), sound(quack).
pet(horse) :- eats(hay), lives_in(barn).
pet(hamster) :- lives_in(cage).

lives_in(barn) :- size(large).
lives_in(house) :- house_broken(yes), size(medium).
lives_in(cage) :- size(small).

sound(X) :- ask('What sound? ', sound, X).
legs(X) :- ask('How many legs? ', legs, X).
eats(X) :- ask('What does it eat? ', eats, X).
size(X) :- ask('What size is it? ', size, X).
house_broken(X) :- ask('Is it house broken? ', house_broken, X).

ask(Prompt, Attr, Val) :-
  known(Attr,X),
  !,
  X = Val.
ask(Prompt, Attr, Val) :-
  (need(Attr, Prompt) ->
  true
  ;
  assert( need(Attr, Prompt) ) ),
  fail.	

init :-
   abolish(known/2).

solve(Attr, Val) :-
   Query =.. [Attr, Val],
   call(Query),
   !.

get_needs(List) :-
   findall( need(Attr, Prompt), retract(need(Attr,Prompt)), List).

add_known(Attr, Val) :-
   assert( known(Attr, Val) ).

main :-
   init,
   dialog.
   
dialog :-
   solve(pet, X),   % if it fails, the next clause gets tried
   write(pet = X),  % if it suceeded, then we're done
   nl.
dialog :-
   get_needs(Needs),
   Needs \= [],
   web_prompt(Needs).
   dialog :-
   write('no answer'),
   nl.

web_prompt(Needs) :-
   batch_questions(Needs, Prompts),
   write(Prompts), nl,
   write('Enter Prolog list with all corresponding answers'), nl,
   write(' (ex. [no, 4, seeds,small]. or [woof].)'), nl,
   read(Answers),
   remember_answers(Needs, Answers),
   dialog.
   
batch_questions([], []).
   batch_questions([need(_,Prompt)|Needs], [Prompt|Prompts]) :-
   batch_questions(Needs, Prompts).

remember_answers([], []).
remember_answers([need(Attr,_)|Needs], [Answer|Answers]) :-
   add_known(Attr, Answer),
   !,
   remember_answers(Needs, Answers).
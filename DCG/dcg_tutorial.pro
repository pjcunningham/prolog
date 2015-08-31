
%% http://www.pathwayslms.com/swipltuts/dcg/

as --> [].
as --> [a], as.

bs --> [].
bs --> [a].
bs --> [a, b], bs.

%% phrase(bs, [a, X, a]).
%% phrase(bs, [a, X, a, Y, a]).
%% phrase(bs, [a, B, A, b, a]).

%% cliche -->
%%     thing,
%%     " is a ", 
%%     type_of_thing, 
%%     " trapped in a ", 
%%     opposite_type_of_thing, 
%%     " body.".
%% thing --> "Cygwin".
%% type_of_thing --> "Unix OS".
%% opposite_type_of_thing --> "Windows'".

cliche(Thing, Type_of_thing, Opposite_type_of_thing) -->
    Thing,
    " is a ", 
    Type_of_thing, 
    " trapped in a ", 
    Opposite_type_of_thing, 
    " body.".

%% queries
%% phrase(cliche("Cygwin", "Unix OS", "Windows'"), L, []), format('~s~n', [L]).
%% phrase(cliche("Fluffy", "dog", "cat's"), L, []), format('~s~n', [L]).
%% phrase(cliche("Bob the swimmer", "fish",  "human"), L, []), format('~s~n', [L]).

fizz_buzz(Msg) --> anything, fizz(Msg), anything, buzz, anything.
anything --> [].
anything --> [_], anything.
fizz(Msg) -->
    "fizz",
    {
        format('At fizz we have Msg=~w~n', [Msg])
    }.
buzz -->
    "buzz".


%% article_phrase --> ("a" ; "an"),
%% 	" ",
%% 	noun.
%% noun --> "book".
%% noun --> "car".    

sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb, noun_phrase.
verb_phrase --> verb.

determiner --> ["the"].
noun --> ["apple"].
noun --> ["man"].

verb --> ["eats"].
verb --> ["sings"].

anything_but_joe --> \+ [joe].

at_end --> \+ [_].

ab_or_c --> [a] -> [b]  ; [c].
ab --> [a] -> [b].

beep_boop --> anything, beep(Suffix), anything, boop(Suffix), anything.

beep(X) -->
    "beep",
    suffix(X).

boop(X) -->
    "boop",
    suffix(X).

suffix([H|T]) -->
      [H],  % The magic - we grab the digit here
      {
          code_type(H, digit)
      },
      suffix(T).
suffix([]) --> []. % must be 2nd suffix clause, or the digits wind up in anything


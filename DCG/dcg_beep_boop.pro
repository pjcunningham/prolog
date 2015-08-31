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

anything --> [].
anything --> [_], anything.

%% phrase(beep(X), [98, 101, 101, 112, 49, 52]).
%% string_codes("14", Suffix), phrase(beep(Suffix), X, []), format('~s~n', [X]).
%% string_codes("trash beep14 foo blah boop14 mep", X), phrase(beep_boop, X).
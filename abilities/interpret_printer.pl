
punct(',').
punct('.').
punct(';').
punct(':').


print(Atom, S0, S1) :-
  punct(Atom), !,
  string_concat(S0,Atom,S1).

print((_,Atom,S0,S2) :-
  string_concat(S0, ' ', S1),
  string_concat(S1,Atom,S2).


printer(Interp,Str) :-
  foldl(print,Interp,([],""),Str).

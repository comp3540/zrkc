:- module(interpret_printer,[printed_text/2]).

punct(',').
punct('.').
punct(';').
punct(':').

capitalization_fix(Before,[FL|T],[FLCaps|T]) :-
	member(Before, [ [], '.', ':' ]),
	char_type(FLCaps,to_upper(FL)).

capitalization_fix(_,A,A).

space_fix([],A,A).
space_fix(_,CharS,[' '|CharS]).

word_fix(Before,Atom,Word) :-
	atom_chars(Atom,Chars),
	capitalization_fix(Before,Chars,Chars1),
	space_fix(Before,Chars1,Chars2),
	string_chars(Word,Chars2).


print(Atom, (_,S0), (Atom,S1)) :-
  punct(Atom), !,
  string_concat(S0,Atom,S1).



print(Atom,(Before,S0),(Atom,S2)) :-
  word_fix(Before,Atom,AtomFixed),
  string_concat(S0,AtomFixed,S2).


printed_text(Interp,Str) :-
  foldl(print,Interp,([],""),(_,Str)).

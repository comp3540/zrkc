:- module(pipeline,[load/2]).
:- use_module(lex).
:- use_module(pipeline/to_json).

load(File,CardsJSON) :-
  read_file_to_string(File,String,[]),
  lex(String,Tokens),
  phrase(cards(Cards),Tokens),
  to_json(Cards,CardsJSON),
  !.




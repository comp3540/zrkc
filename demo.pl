:- use_module(library(pprint)).

:- use_module(lex).
:- use_module(cards).
:- use_module(abilities/abilities).
:- use_module(abilities/interpret).
:- use_module(abilities/interpret_printer).
:- use_module(pipeline/pipeline).

parse_card(String,AST) :-
	lex(String,Tokens),
	phrase(card(AST),Tokens,[]), !.

parse_ability(String,AST) :-
	lex(String, Tokens),
	phrase(ability(AST),Tokens,[]), !.

print_card(String) :-
  parse_card(String,AST),
  print_term(AST,[]).

print_ability(String) :-
	parse_ability(String,AST),
	print_term(AST,[]).

demo(String) :-
  parse_ability(String,AST),
  print_term(AST,[]), nl,
  interpret_ability(AST,Interp),
  writeln(Interp), !.  

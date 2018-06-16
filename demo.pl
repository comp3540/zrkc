:- use_module(library(pprint)).

:- use_module(lex).
:- use_module(cards).
:- use_module(abilities/abilities).
:- use_module(abilities/interpret).


parse_card(String,AST) :-
	lex(String,Tokens),
	phrase(card(AST),Tokens,[]), !.

parse_ability(String,AST) :-
	lex(String, Tokens),
	phrase(ability(AST),Tokens,[]), !.

print_ability(String) :-
	parse_ability(String,AST),
	print_term(AST,[]).

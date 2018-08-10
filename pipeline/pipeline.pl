:- module(pipeline,[load_cards/2, load_interpret/2]).
:- use_module(lex).
:- use_module(pipeline/to_json).
:- use_module(library(http/json)).
:- use_module(cards).
:- use_module(abilities/abilities).
:- use_module(abilities/interpret).


parse_from_file(Parser,File,AST) :-
  read_file_to_string(File,String,[]),
  lex(String,Tokens),
  call(Parser,AST,Tokens,[]),!.


load_cards(File,JSON) :-
  parse_from_file(cards,File,AST),
  to_json(AST,JSON).


interpret_export(Ability,ExportDict) :-
  Ability = ability(name(Name),_),
  writeln(Name),
  interpret_ability(Ability,Text),
  ExportDict = @{name: Name, interpretation: Text}.

load_interpret(File,JSON) :-
  parse_from_file(abilities,File,AST),
  maplist(interpret_export,AST,AbilityDefs),
  atom_json_term(JSON,AbilityDefs,[as(string)]).
  



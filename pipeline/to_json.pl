:- module(to_json,[to_json/2]).
:- use_module(library(http/json)).

to_json(Struct,JSONString) :-
  dictify_list(Struct,DictList),
  atom_json_term(JSONString,DictList,[as(string)]).

% We lazily give an alias for the clause
% that processes lists
dictify_list(List,ListJSON) :-
  dictify_args([List],ListJSON).

% Replace our arbitrary Prolog atoms with the JSON null-type
literal_to_json(nil,@null).
% Pass-through: Prolog already knows about the rest
literal_to_json(V,V).


% F(values) => F: json
% We don't yet know what should be done with values,
% or even what's in them
term_to_tuple(Term,Key=Values) :-
  Term =.. [Key|Args],
  dictify_args(Args,Values).


% RHS is a Prolog non-compound => JSON literal
% e.g. atoms, numbers, strings
dictify_args([Literal],LiteralJSON) :-
  Literal =.. [Literal],
  literal_to_json(Literal,LiteralJSON).


% RHS is a Prolog list => JSON list
% If values was just one list, it stays a list
% The terms in the list are each their own objects
dictify_args([ListOfTerms],ListOfObjects) :-
  is_list(ListOfTerms),
  maplist(term_to_object,ListOfTerms,ListOfObjects).


% RHS is a bunch of Prolog arguments => JSON object
% If values are a bunch of other terms, we create one object for them,
% treating the functor as key, and once again the values may vary
dictify_args(ListOfTerms,SingleObject) :-
  maplist(term_to_tuple,ListOfTerms,ListOfTuples),
  dict_create(SingleObject,@,ListOfTuples).


% Element is a Prolog term => JSON object
term_to_object(Term,Object) :-
  compound(Term),
  term_to_tuple(Term,Tuples),
  dict_create(Object,?,[Tuples]).


% Element is a Prolog non-compound => JSON literal 
term_to_object(Literal,LiteralJSON) :-
    literal_to_json(Literal,LiteralJSON).

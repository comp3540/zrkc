:- module(to_json,[to_json/2]).
:- use_module(library(http/json)).

to_json(Struct,JSONString) :-
  dictify_args([Struct],DictList),
  atom_json_term(JSONString,DictList,[as(string)]).

json_map(nil,@null).
json_map(V,V).

% A compound term is Functor:Values, with Values being a single dict
dictify_term(Term,Key=Values) :-
  compound(Term),
  Term =.. [Key|Args],
  dictify_args(Args,Values).

dictify_term(Raw,RawJSON) :-
  json_map(Raw,RawJSON).

% A single list argument uses dictify_pack
% A list becomes a list of dictionaries
dictify_args([List],DictList) :-
  is_list(List),
  maplist(dictify_pack,List,DictList).

% A single argument is represented by itself
dictify_args([Arg],Arg1) :-
  Arg =.. [Arg],
  dictify_term(Arg,Arg1).

% The arguments to a compound term form a single dict
% Multiple arguments: one dictionary, key:value
dictify_args(Args,Dict) :-
  maplist(dictify_term,Args,Tuples),
  dict_create(Dict,@,Tuples).

% Dictify pack
dictify_pack(Term,Dict) :-
  compound(Term),
  dictify_term(Term,Tuples),
  dict_create(Dict,?,[Tuples]).

dictify_pack(Val,TermVal) :-
  dictify_term(Val,TermVal).

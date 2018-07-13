:- use_module(lex).

load(File,Cards) :-
  read_file_to_string(File,String,[]),
  lex(String,Tokens),
  phrase(cards(Cards),Tokens),
  !.

dictify_1(Term,Dl) :-
  is_list(Term),
  dictify(Term,Dl).

dictify_1(Term,Dict) :-
 Term =.. [Functor|Args],
 dictify(Args,Args1),
 dict_create(Dict,@,[Functor=Args1]).

dictify_1(_,error).

dictify([T],T) :-
    T =.. [T].

dictify(List,Fixed) :-  maplist(dictify_1,List,Fixed).

/*dictify_list(List,Tuples) :-
  maplist(dictify_term,List,Tuples).

% Lists restart each process
dictify_term(L,Tuples) :-
  is_list(L),
  write('Actual list'),
  writeln(L),
  maplist(dictify_args(L,Tuples),
  writeln(Tuples).

% Identify compound terms
dictify_term(Term,Key=Values) :-
  write('term>'),writeln(Term),
  Term =.. [Key|Args],
  dictify_args(Args,Values).



% Simple terms
dictify_args([Arg],Arg) :-
  Arg =.. [Arg].

% Compound terms (process each argument separately)
dictify_args(Values,Args) :-
  writeln(Args),
  dictify_list(Args,Tuples),
  dict_create(Values,@,Tuples).*/

% A true list is a list of dictionaries of each of its values
/*dictify_term(List,DictList) :-
  is_list(List),
  maplist(dictify_pack,List,DictList).*/

json_map(nil,@(null)).
json_map(A,A).

% A compound term is Functor:Values, with Values being a single dict
dictify_term(Term,Key=Values) :-
  Term =.. [Key|Args],
  dictify_args(Args,Values).

% A single list argument uses dictify_pack
dictify_args([List],DictList) :-
  is_list(List),
  maplist(dictify_pack,List,DictList).

% A single argument is represented by itself
dictify_args([Arg],ArgJson) :-
  json_map(Arg,ArgJson),
  Arg =.. [Arg].

% The arguments to a compound term form a single dict
dictify_args(Args,Dict) :-
  maplist(dictify_term,Args,Tuples),
  dict_create(Dict,@,Tuples).

% Dictify pack
dictify_pack(Term,Dict) :-
  dictify_term(Term,Tuples),
  dict_create(Dict,?,[Tuples]).

/**
 *
 * [H|T] => [H|T] (actual lists)
 * f(5) ==> f:5
 * f(5,6) ==> does not work
 * f(g(5),h(6)) ==> f:@{g:5, h:6}
 */

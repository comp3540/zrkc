flip_run(N,cond(if(flip),True,False),cond(if(flip(N),True,False))).

/**
 * rle(+Pattern, +Input:list, -Encoded:list) is det. 
 * Performs run-length encoding on the Input list, yielding the Encoded list.
 * The predicate Pattern is used to represent runs. 
 *
 * Pattern(?N:int,?X,?Run) is semidet.
 * Generates a representation in Run, for a run of N values X. May fail to
 * indicate that RLE should not be be performed for value X.
 */


rle(Pattern,[V|In], Out) :-
  rle1(Pattern,In, V, Out).

rle1(_,[], Acc, [Acc]).

rle1(Pattern,[X|In], X, Out) :-
  call(Pattern,2,X,Run), !,
  rle1(Pattern, In,Run,Out), !.

rle1(Pattern,[X|In], Acc, Out) :-
  call(Pattern,N,X,Acc), !,
  succ(N,NPrime),
  call(Pattern,NPrime,X,Run),
  rle1(Pattern, In,Run,Out), !.
 
rle1(Pattern, [V|In], Acc, [Acc|Out]) :-
  rle1(Pattern, In, V, Out).

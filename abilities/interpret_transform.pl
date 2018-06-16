relates_to_opponent(draw(opponent,_)).
relates_to_opponent(shuffle(opponent)).
relates_to_opponent(deck(opponent,_,_,_)).


opponent_context_(Action, 
  (Buf,[Action|OpponentActionS],ActionS), 
  (Buf, OpponentActionS, ActionS)
) :-
  relates_to_opponent(Action).

opponent_context_(Action,(Buf,[],BufRes), (opponent_context([Action|OpponentActionS]), OpponentActionS, ActionS)) :-
  (Buf = nil -> BufRes = ActionS; BufRes = [Buf|ActionS]),
  relates_to_opponent(Action).

opponent_context_(Action,(Buf,[],BufRes), (Action, [], ActionS)) :-
  (Buf = nil -> BufRes = ActionS; BufRes = [Buf|ActionS]).

opponent_context(ActionS, Contextualized) :-
  foldl(opponent_context_,ActionS,(nil,[],Contextualized), (LastAction,[],[LastAction])).



%

glue(A,L,[A|L]) :- is_list(L).
glue(A,V,[A|[V]]).

merge_applystat_(
      applystatus(Status, Target),
      (applystatus(StatusS, Target), ActionS), 
      (applystatus(StatusList, Target), ActionS)
) :-
  glue(Status,StatusS,StatusList).

merge_applystat_(Action, (Buf, [Buf|ActionS]), (Action, ActionS)).

merge_applystat([Action|ActionS],Merged) :-
    foldl(merge_applystat_,ActionS,(Action,Merged),(LastAction,[LastAction])).





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

rle_(X, (Pattern,Run0,RunS), (Pattern,Run1,RunS)) :- 
  call(Pattern, N0, X, Run0), 
  succ(N0,N1), 
  call(Pattern, N1, X, Run1), !.

rle_(X, (Pattern,X,RunS), (Pattern,X2,RunS)) :- call(Pattern, 2, X, X2), !.
rle_(V, (Pattern,Run,[Run|RunS]), (Pattern,V,RunS)) :- V \= Run, !.

rle(Pattern,[V|VS],Encoded) :- foldl(rle_,VS,(Pattern,V,Encoded),(Pattern,LastRun,[LastRun])).

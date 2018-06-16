
merge_applystat(A,T,[A|T]).
merge_applystat(applystat(Status,Target),[applystat(StatusS,Target)|ActionS],[applystat([Status|StatusS],Target)|ActionS]).



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

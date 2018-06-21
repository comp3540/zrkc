:- module(interpret_transform,[prepare_ability/2]).

prepare_ability(ability(Name,ActionS),ability(Name,TActionS)) :-
	traversal(ActionS,TActionS).

transform(Action0,ActionF) :-
	rle(flip_run,Action0,Action1),
	merge_applystat(Action1,Action2),
	opponent_context(Action2,ActionF).

traversal_action(add(Who,Trigger,ActionS), add(Who,Trigger,TActionS)) :-
	traversal(ActionS,TActionS), !.

traversal_action(cond(If,then(True),else(False)), cond(If,then(TTrue),else(TFalse))) :-
	traversal(True,TTrue),
	traversal(False,TFalse), !.

traversal_action(Action,Action).

traversal_list([],[]).

traversal_list([Action|ActionS],[TransformedAction|TransformedActionS]) :-
	traversal_action(Action,TransformedAction),
	traversal_list(ActionS,TransformedActionS).

traversal(null,null) :- !.

traversal(L, Res) :-
	transform(L,Transformed),
	traversal_list(Transformed,Res).


foldr(Reduction, ListI, ListO) :-
	reverse(ListI,[Last|Rest]),
	foldl(Reduction, Rest, [Last|[]], ListO).

opponent_context(ActionS,Result) :-
	foldr(opponent_context_, ActionS, Result).

relates_to_opponent(draw(opponent,_)).
relates_to_opponent(shuffle(opponent)).
relates_to_opponent(deck(opponent,_,_,_)).

opponent_context_(OpponentAction, AccI, AccO) :- 
	relates_to_opponent(OpponentAction),
	AccI = [opponent_context(OpponentActionS)|OtherActionS],
	AccO = [opponent_context([OpponentAction|OpponentActionS])|OtherActionS].

opponent_context_(OpponentAction, AccI, AccO) :-
	relates_to_opponent(OpponentAction),
	AccO = [opponent_context([OpponentAction])|AccI].

opponent_context_(Action, AccI, AccO) :-
	AccO = [Action|AccI].



%

glue(A,L,[A|L]) :- is_list(L).
glue(A,V,[A|[V]]).

merge_applystat_(See, Args, Result) :- 
  	See = applystatus(Status,Target),
	Args = (applystatus(StatusS,Target), ActionS),
	glue(Status,StatusS,StatusList),
	Result = (applystatus(StatusList,Target), ActionS),
	!.

merge_applystat_(Action, Args, Result) :-
	% It's not an applystat
	Args = (BufAction, [BufAction|ActionS]),
	Result = (Action, ActionS),
	!.

merge_applystat([Action|ActionS],Head) :-
	Initial = (Action,Head),
    	foldl(merge_applystat_,ActionS,Initial,Result),
	Result = (LastAction,[LastAction|[]]).





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

% Incrementing count > 2
rle_(X, Args, Result) :-
  Args = (Pattern, XN0, RunS), 
  call(Pattern, N0, X, XN0), 
  succ(N0,N1), 
  call(Pattern, N1, X, XN1),
  Result = (Pattern, XN1, RunS), !.

% 2 of the same
rle_(X, Args, Result) :- 
	Args = (Pattern, X, RunS),
	call(Pattern, 2, X, X2), 
	Result = (Pattern, X2, RunS), !.

% When I see some value V, and it doesn't equal the Run value, then I copy Run onto the answers,
% because I know that Run is finished. Any other runs will use the same Pattern function, because we don't change it
% Any other runs have to start with V, and they'll have to be written in the slot after Run, which we call RunS. 
rle_(V, Args, Result) :- 
	Args = (Pattern, Run, [Run|RunS]),
	Result = (Pattern, V, RunS),
	!.

rle(Pattern,[V|VS],Head) :- 
	Initial = (Pattern, V, Head),
	foldl(rle_,VS,Initial,Final),
	Final = (Pattern, LastRun, [LastRun|[]]).

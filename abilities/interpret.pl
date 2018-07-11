:- module(interpret,[interpret_ability/2,spacey/1]).
:- use_module(interpret_transform).


spacey(Interpretation) :-
	atomics_to_string(Interpretation,' ',String),
	writeln(String).

interpret_ability(Ability,Interpretation) :-
	prepare_ability(Ability,Ability1),
	phrase(interpret_ast(Ability1), Interpretation),
	!.

interpret_ast(ability(name(Name),ActionS)) --> [Name,':'], interpret_list(ActionS).

interpret_list_t(PrecedingAction,[]) --> clause_ender(PrecedingAction).
interpret_list_t(PrecedingAction,ActionS) --> clause_joiner(PrecedingAction), interpret_list(ActionS).
interpret_list([Action|ActionS]) --> interpret(Action), interpret_list_t(Action,ActionS).

interpret(Action) --> dam(Action).
interpret(Action) --> heal(Action).
interpret(Action) --> deenergize(Action).
interpret(Action) --> draw(Action).
interpret(Action) --> shuffle(Action).
interpret(Action) --> null(Action).
interpret(Action) --> destat(Action).
interpret(Action) --> add(Action).
interpret(Action) --> deck(Action).
interpret(Action) --> reenergize(Action).
interpret(Action) --> redamage(Action).
interpret(Action) --> swap(Action).
interpret(Action) --> search(Action).
interpret(Action) --> cond(Action).
interpret(Action) --> applystat(Action).
interpret(Action) --> opponent_context(Action).

clause_joiner(cond(_,_,_)) --> [].
clause_joiner(opponent_context(_)) --> [].
clause_joiner(_) --> [';'].


clause_ender(cond(_,_,_)) --> [].
clause_ender(opponent_context(_)) --> [].
clause_ender(_) --> ['.'].


opponent_context(opponent_context(Actions)) --> [the,opponent], interpret_list(Actions).

cond(cond(if(flip),then(T),else(F))) --> flip(T,F).
cond(cond(if(flip(Times)),then(T),else(F))) --> multiflip(Times,T,F).
cond(cond(if(choice),then(T),else(F))) --> choice(T,F).
cond(cond(if(you_do(TrueH)),then(TrueT),else(null))) --> {append(TrueH,TrueT,True)}, you_do(True).
cond(cond(if(you_do(TrueH)),then(TrueT),else(F))) --> {append(TrueH,TrueT,True)}, choice(True,F).
cond(cond(if(healed(Target)), then(T), else(F))) --> healed(Target,T,F).
cond(cond(if(ineq(Ineq)), then(T), else(F))) --> ineq(Ineq,T,F).

multiflip(Times,T,F) --> [flip,a,coin,Times,times,'.',each,time,you,get,heads,','], interpret_list(T), multiflip_else(F).
multiflip_else(null) --> [].
multiflip_else(F) --> [each,time,you,get,tails,','], interpret_list(F).


flip(T,F) --> [flip,a,coin,'.',if,heads,','], interpret_list(T), flip_else(F).
flip_else(null) --> [].
flip_else(F) --> [if,tails,','], interpret_list(F).

choice(T,F) --> [if,you,so,choose,','], interpret_list(T), choice_else(F).
choice_else(null) --> [].
choice_else(F) --> [if,not,','], interpret_list(F).

you_do(T) --> [if,you,choose,to,use,this,ability], interpret_list(T).
% How do you know which actions aren't in a condition if the condition is in the middle.
%

healed(target(Target),T,F) --> [if], target(Target), [was,healed,during,this,turn,then], interpret_list(T), healed_else(F).
healed_else(null) --> [].
healed_else(F) --> [else], interpret_list(F).

ineq(Ineq,T,F) --> {Ineq =.. [Op,A,B]}, [if], ineq_object(A,Property), ineq_comparison(Op,B), ineq_property(Property) , [',', then], interpret_list(T),  ineq_else(F).
ineq_else(null) --> [].
ineq_else(F) --> [otherwise,','], interpret_list(F).

ineq_object(count(target(Target)),pokemon) --> target(Target).
ineq_object(count(target(Target),filter(Filter)), Filter) --> target(Target).

ineq_comparison(>,0) --> [has,any].
ineq_comparison(>=,1) --> [has,any].
ineq_comparison(=/=,0) --> [has,any].

ineq_comparison(<,1) --> ['doesn`t',have,any].
ineq_comparison(=,0) --> ['doesn`t',have,any].

ineq_comparison(=,N) --> [has,exactly,N].
ineq_comparison(>=,N) --> [has,at,least,N].
ineq_comparison(=<,N) --> [has,at,most,N].
ineq_comparison(>,N) --> [has,more,than,N].
ineq_comparison(<,N) --> [has,less,than,N].
ineq_comparison(=/=,N) --> [has,any,amount,other,than,N].

ineq_property(pokemon) --> [pokemon].
ineq_property(Filter) --> filter(Filter).

% If your active pokemon has more damage than the opponent's active pokemon has energy, then
% OBJECT1 BICOMPARISON PROPERTY1 than OBJECT2 has PROPERTY 2
% If your active has more damage than the opponent's active pokemon has, then
% OBJECT BICOMPARISON PROPERTY0 than OBJECT2 has
% 5 > xxx



dam(dam(target(opponent_active),hp(HP))) --> [do,HP,damage], { number(HP) }.
dam(dam(target(Affected),hp(HP))) --> 
  [do], count(HP,arbitrary,units(damage,damage),Var), 
  [to], target(Affected), 
  quant(Var) /* for every... */.

heal(heal(target(Affected),hp(HP))) --> 
  [heal], count(HP,[target(Affected),filter(damage)],units(damage,damage),Var),
  [from], target(Affected),
  quant(Var) /* for every... */.

deenergize(deenergize(target(Affected),energy(Energy))) --> 
  [remove], count(Energy,                         % N               
    [target(Affected),filter(energy(colorless))], % out of all the energy on the affected cards
    units('energy card','energy cards')           % measured in units of energy cards
   ,Var),                                         % in proportion to some target (if applicable)
  [from], target(Affected),
  quant(Var).


draw(draw(your,Ct)) --> 
  [draw], count(Ct,arbitrary,units(card,cards),Counted),
  quant(Counted).

draw(draw(opponent,Ct)) --> 
  [draws], count(Ct,arbitrary,units(card,cards),Counted),
   quant(Counted).

shuffle(shuffle(your)) --> [shuffle,your,deck].
shuffle(shuffle(opponent)) --> [shuffles,their,deck].

null(null) --> [do,nothing].

destat(destat(target(Target))) --> [all,status,effects,are,now,removed,from], target(Target).

add(add(_,trigger(Cause,Moment),ActionList)) --> [each,time], trigger(Cause,Moment), [','], interpret_list(ActionList).

trigger(opponent,turnend) --> [the,opponent,ends,their,turn].
trigger(your,turnend) --> [you,end,your,turn].

deck(deck(Who,destination(Where,Placement),choice(Choice),Ct)) -->
  person_form(Who,v(place,places)),    % Whose deck?
  deck_count(Who,Choice,Ct,Var),                      % How many cards (and who chooses)
  placement_action(Placement),                        % Top or bottom
  deck_dest(Who,Where),                               % Deck or discard
  quant(Var).                                         % for every...


% Does not use count, to get rid of choice when there is no choice
deck_count(your,_,count(target(your_hand)),[]) --> [all,the,cards,in,your,hand].
deck_count(opponent,_,count(target(opponent_hand)),[]) --> [all,the,cards,in,their,hand].

deck_count(Person,Person,Target,Inner) --> count(Target,arbitrary,units(card,cards),Inner), [from], person_possesive(Person,hand).
deck_count(Affected,Choosing,Target,Inner) --> count(Target,arbitrary,units(card,cards), Inner), of_choice(Choosing), [from], person_possesive(Affected,hand).

of_choice(your) --> [of,your,choice].
of_choice(opponent) --> [of,the,'opponent`s',choice].
 

reenergize(reenergize(target(Source),target(Dest),energy(Amount))) -->
   [move], count(Amount,[target(Source),filter(energy(colorless))],units(energy,energy),Var), 
   [from], target(Source),
   [to], same(another,Source,Dest),target(Dest),
   quant(Var).


redamage(redamage(source(Source),destination(Dest),hp(Amount))) -->
  [distribute], count(Amount,[target(Source),filter(damage)],units('damage counter','damage counters'),Var),
  [from], target(Source),
  [among], same_collection('the rest of',Source,Dest), target(Dest),
  quant(Var).

swap(swap(source(From),destination(To))) --> [switch], target(From), [with], same_collection(another,From,To), target(To), swap_note(From).

swap_note(your_active) --> ['.',all,status,effects,are,removed,from,that,pokemon].
swap_note(opponent_active) --> ['.',all,status,effects,are,removed,from,that,pokemon].
swap_note(_) --> [].


same_collection(ClarifyingWord,choice(T),T) --> [ClarifyingWord].
same_collection(ClarifyingWord,T,choice(T)) --> [ClarifyingWord].
same_collection(ClarifyingWord,T,U) --> same(ClarifyingWord,T,U).

same(ClarifyingWord,T,T) --> [ClarifyingWord].
same(_,_,_) --> [].

placement_action(top) --> [onto].
placement_action(bottom) --> [at,the,bottom,of].

deck_dest(Person,deck) --> person_possesive(Person,deck).
deck_dest(Person,discard) --> person_possesive(Person,'discard pile').



person_possesive(opponent,Object) --> [their,Object].
person_possesive(your,Object) --> [your,Object].

person_long(opponent,Object) --> ['the opponent`s',Object].
person_long(your,Object) --> [your,Object].


person_form(your,v(First,_)) --> [First].
person_form(opponent,v(_,Second)) --> [Second].




quant([]) --> [].
quant([target(Target)]) --> ['for every card in'], target(Target).
quant([target(Target),filter(Filter)]) --> ['for every'], filter_object(Filter), ['on'], target(Target). 


search(search(Whose,source(Source),filter(SearchFilter),Amount)) --> [look], search_scope(SearchFilter), search_source(Whose,Source), search_take(Amount,SearchFilter).

search_scope(placement(Where,1)) --> [at,the], placement_word(Where), [card,of].
search_scope(placement(Where,N)) --> [through,the], placement_word(Where), [N,cards,of].
search_scope(_) --> [through].

placement_word(top) --> [top].
placement_word(bottom) --> [bottom].


search_source(Person,deck) --> person_long(Person,deck).
search_source(Person,discard) --> person_long(Person,'discard pile').

search_take(0,_) --> [].
search_take(Amount,placement(_,_)) --> [',',then,take], count(Amount,arbitrary,units(card,cards),[]).
search_take(Amount,Filter) --> {filter(Filter,Sing,[card]), filter(Filter,Pl,[cards])}, [',',then,take], count(Amount,arbitrary,units(Sing,Pl),Var), quant(Var).


applystat(applystatus(Status,target(Target))) --> target(Target), [is,now], list(Status).

% Missing evolves from and fake target your-pokemon
filter(energy(EnergyType)) --> energy_card(EnergyType), [energy].
filter(trainer(Type)) --> trainer_card(Type).
filter(pokemon(Stage)) --> pokemon_card(Stage), [pokemon].
filter(damage) --> [damage]. % damage counters
filter(evolves_from(choice(dChoice,Target))) --> [that,evolves,from], target(Target).

trainer_card(item) --> [item].
trainer_card(supporter) --> [supporter].
trainer_card(stadium) --> [stadium].

pokemon_card(any) --> [].
pokemon_card(basic) --> [basic].
pokemon_card(stageone) --> [stage,one].


filter_object(energy(EnergyType)) --> energy_card(EnergyType), [energy,card].
filter_object(damage) --> [damage,counter].

energy_card(colorless) --> [].
energy_card(psychic) --> [psychic].

count(N, _, Units, []) --> { number(N) }, count_integer(N, Units).
count(N*Count, OutOf , Units, CountArg) -->  { number(N) }, count_variable(N,Count,OutOf, Units, CountArg).
count(Count*N, OutOf, Units, CountArg) --> { number(N) }, count_variable(N,Count,OutOf,Units,CountArg).
count(Count,OutOf,Units,CountArg) --> count_variable(1,Count,OutOf,Units,CountArg).


count_integer(1,units(Sing,_)) --> [1,Sing].
count_integer(N,units(_,Pl)) --> [N,Pl].

% This rule deals with cases when the count in fact refers to the entire thing upon which we act
% e.g. remove all of the energy from something, discarding all of the cards
count_variable(1,Count,CountArgs,units(_,Pl),[]) --> {Count =.. [count|CountArgs]}, [all,of,the,Pl].

% This rule deals with cases where we are asked to count the "last" or "last source" of a command,
% and we are able to determine that this refers to the entire thing upon which we act
% e.g. the damage of the "last source", where the command aims to move the damage of the "source" (i.e. redamage)
count_variable(1,Count,OutOf,units(_,Pl),[]) --> {
  OutOf \= arbitrary,
  Count =.. [count|[target(LastRef)|CountRest]],        % When we're asked to count a target, and potentially use filters
  check_last_ref(LastRef),                              % And this target is a "last" target
  OutOf = [target(_)|CountRest] },                      % And we're counting out of a target (i.e. this is what last is talking about), and the filters match
  [all,of,the,Pl].                                      % Then, this is a circuitous reference to the entire value that this expression is out of.

count_variable(N,Count,_,Units,CountArgs) --> {Count =.. [count|CountArgs]}, count_integer(N,Units).

% cond:ability without else: If you choose to use this ability
% cond:ability with else: becomes cond choice
%

% target, filter: target has more than N filter
% target: target has more than N pokemon

%% your-active, energy: your active pokemon has more than 5 energy cards
%% your-bench: your bench has more than 5 pokemon

target([]) --> [].
target(your_active) --> ['your active pokemon'].
target(opponent_active) --> ['the opponent`s active pokemon'].
target(your_hand) --> ['your hand'].
target(your_bench) --> ['your bench'].
target(last) --> ['that pokemon'].
target(self) --> ['the pokemon to which this card is attached'].
target(choice(your)) --> ['one of your pokemon'].
target(choice(opponent)) --> ['one of the opponent`s pokemon'].
target(choice(your_bench)) --> ['one of your benched pokemon'].
target(choice(opponent_bench)) --> ['one of your opponent`s benched pokemon'].
target(opponent) --> ['the opponent`s pokemon'].
target(last_source) --> ['that pokemon'].

check_last_ref(Potential) :- memberchk(Potential,[last,last_source]).


list_t([Last]) --> ['and'], [Last].
list_t([H|T]) --> [','], [H], list_t(T). 

list([H|T]) --> [H], list_t(T).
list([Single]) --> [Single].
list(Single) --> [Single].

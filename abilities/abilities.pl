:- module(abilities,[abilities//1,ability//1]).
:- use_module(lex).
:- use_module(cards).
:- use_module(arith).

abilities_t([]) --> [].
abilities_t(L) --> [nl], abilities(L). 

abilities([H|T]) --> ability(H), abilities_t(T).

ability(ability(name(Name),Actions)) --> [string(Name),colon], action_list(Actions).

action_list_t([]) --> [].
action_list_t(L) --> [comma], action_list(L).
action_list([H|T]) --> action(H), action_list_t(T).

action(Act) --> cond(Act).
action(Act) --> damage(Act).
action(Act) --> heal(Act).
action(Act) --> redamage(Act).
action(Act) --> deenergize(Act).
action(Act) --> reenergize(Act).
action(Act) --> applystat(Act).
action(Act) --> destat(Act).
action(Act) --> draw(Act).
action(Act) --> shuffle(Act).
action(Act) --> null(Act).
action(Act) --> deck(Act).
action(Act) --> add(Act).
action(Act) --> swap(Act).
action(Act) --> search(Act).

null(null) --> id(null).
cond(cond(if(Test),then(True),else(False))) --> id(cond), [colon], test(Test), [colon], branch(True), branch_option(False).

branch_option(null) --> [].
branch_option(Branch) --> [colon], id(else), [colon], branch(Branch).

test(flip) --> id(flip).
test(choice) --> id(choice).
test(ineq(Ineq)) --> ineq(Ineq).
test(you_do(Ability)) --> id(ability), [colon], branch(Ability).
test(healed(Target)) --> id(healed), [colon], target(Target).

branch([Action]) --> action(Action).
branch(Actions) --> [lparen], action_list(Actions), [rparen].

damage(dam(Target,HP)) --> id(dam), [colon], target(Target), [colon], hp(HP).
redamage(redamage(Source,destination(Destination),HP)) --> id(redamage), [colon], swap_source(Source), [colon], id(destination), [colon], person_target(Destination), [colon], hp(HP).
heal(heal(Target,HP)) --> id(heal), [colon], target(Target), [colon], hp(HP).
deenergize(deenergize(Target,NCards)) --> id(deenergize), [colon], target(Target), [colon], energy_cards(NCards).
reenergize(reenergize(Source,Dest,NCards)) --> id(reenergize), [colon], target(Source), [colon], energy_cards(NCards), [colon], target(Dest), [colon], energy_cards(NCards).

hp(hp(HP)) --> arith(HP).
energy_cards(energy(NCards)) --> arith(NCards).

applystat(applystatus(Status,target(Target))) --> id(applystat), [colon], status(Status), [colon], target_value(Target).
destat(destat(Target)) --> id(destat), [colon], target(Target).

status(Status) --> id(status), [colon], status_type(Status).

status_type(poisoned) --> id(poisoned).
status_type(paralyzed) --> id(paralyzed).
status_type(stuck) --> id(stuck).
status_type(asleep) --> id(asleep).

draw(draw(DrawTarget,Amount)) --> id(draw), draw_target(DrawTarget), [colon], arith(Amount).
draw_target(your) --> [].
draw_target(opponent) --> [colon], id(opponent).

shuffle(shuffle(ShuffleTarget)) --> id(shuffle), [colon], id(target), [colon], person_target(ShuffleTarget).

person_target(your) --> id(your).
person_target(opponent) --> id(opponent).

deck(deck(DeckTarget,DeckDest,DeckChoice,Amount)) --> id(deck), [colon], id(target), [colon], person_target(DeckTarget), [colon], deck_dest(DeckDest), deck_choice_option(DeckChoice), [colon], arith(Amount).


deck_choice_option(choice(your)) --> [].
deck_choice_option(DeckChoice) --> [colon], id(choice), [colon], deck_choice(DeckChoice).


deck_dest(destination(To,Position)) --> id(destination), [colon], deck_dest_to(To), placement_strategy_option(Position).
deck_dest_to(deck) --> id(deck).
deck_dest_to(discard) --> id(discard).

placement_strategy_option(top) --> [].
placement_strategy_option(O) --> [colon],  placement_strategy(O).

placement_strategy(top) --> id(top).
placement_strategy(bottom) --> id(bottom).

deck_choice(choice(your)) --> id(you).
deck_choice(choice(opponent)) --> id(them).

add(add(Target, Trigger, Actions)) --> id(add), [colon], id(target), [colon], person_target(Target), [colon], trigger(Trigger), [colon, lparen], action_list(Actions), [rparen].
trigger(trigger(Cause,Moment)) --> id(trigger), [colon], trigger_cause(Cause), [colon], trigger_moment(Moment).
trigger_cause(opponent) --> id(opponent).
trigger_moment(turnend) --> id(turn-end).

% swap, search, filters, arith, count, target, target-value, gen-source, gen-dest, reenergize, redamage
swap(swap(Source,Dest)) --> id(swap), [colon], swap_source(Source), [colon], swap_dest(Dest).
swap_source(source(TargetValue)) --> id(source), [colon], target_value(TargetValue).
swap_dest(destination(TargetValue)) --> id(destination), [colon], target_value(TargetValue).

search(search(Target, SearchSource,Filter,Amount)) --> id(search), [colon], id(target), [colon], tv_wally(choice(Target,InnerFilter)), [colon], search_source(SearchSource), [colon], search_wally_filter(Filter,choice(Target-InnerFilter)), [colon], arith(Amount).

search(search(Target,SearchSource,Filter,Amount)) --> id(search), [colon], id(target), [colon], person_target(Target), [colon], search_source(SearchSource), [colon], search_filter(Filter), [colon], arith(Amount).

search_source(source(DDT)) --> id(source), [colon], deck_dest_to(DDT).
search_filter(filter(Filter)) --> id(filter), [colon], searchable_filter(Filter).

search_wally_filter(filter(Filter),InnerFilter) --> id(filter), [colon], search_wally_filter_tail(Filter,InnerFilter).
search_wally_filter_tail(Filter,InnerFilter) --> evolution_filter(Filter,InnerFilter).
search_wally_filter_tail(Filter,_) --> searchable_filter(Filter).


searchable_filter(F) --> placement_filter(F).
searchable_filter(F) --> selection_filter(F).
searchable_filter(F) --> evolution_filter(F,idk).

placement_filter(placement(Strategy,Length)) --> placement_strategy(Strategy), search_length(Length).
search_length(all) --> [].
search_length(N) --> [colon], arith(N).

selection_filter(F) --> energy_filter(F).
selection_filter(F) --> pokemon_filter(F).
selection_filter(F) --> trainer_filter(F).

energy_filter(energy(Type)) --> id(energy), energy_filter_t(Type).
energy_filter_t(colorless) --> [].
energy_filter_t(Type) --> [colon], energy_type(Type).

trainer_filter(trainer(Cat)) --> id(cat), [colon], trainer_type(Cat).

pokemon_filter(pokemon(Type)) --> id(pokemon), pokemon_filter_tail(Type).
pokemon_filter_tail(any) --> [].
pokemon_filter_tail(Evolution) --> [colon], id(cat), [colon], pokemon_type(Evolution).
pokemon_type(basic) --> id(basic).
pokemon_type(stageone) --> id(stage-one).

evolution_filter(evolves_from(EvTarget),InnerFilter) --> id(evolves-from), [colon], id(target), [colon], evolution_target(EvTarget,InnerFilter).
evolution_target(last,idk) --> id(last).
evolution_target(IF,IF) --> id(last).

arith:count_expr(count(Target)) --> target(Target).
arith:count_expr(count(target(Target))) --> target_value(Target).
arith:count_expr(count(Target, filter(CountableFilter))) --> target(Target), [colon], countable_filter(CountableFilter).
arith:count_expr(count(Target, filter(DamageCount))) --> target(Target), [colon], damage_count(DamageCount).

countable_filter(F) --> energy_filter(F).
damage_count(damage) --> id(damage).

target(target(TV)) --> id(target), [colon], target_value(TV).
target_value(TV) --> tv_direct(TV).
target_value(choice(TV)) --> id(choice), [colon], tv_choice(TV).
target_value(TV) --> tv_last(TV).

tv_direct(your_active) --> id(your-active).
tv_direct(opponent_active) --> id(opponent-active).
tv_direct(your_hand)  --> id(your-hand).
tv_direct(opponent_hand) --> id(opponent-hand).
tv_direct(your_bench) --> id(your-bench).
tv_direct(opponent_bench) --> id(opponent-bench).
tv_direct(self) --> id(self).

tv_choice(opponent) --> id(opponent).
tv_choice(your) --> id(your).
tv_choice(opponent_bench) --> id(opponent-bench).
tv_choice(your_bench) --> id(your-bench).
tv_choice(your_hand) --> id(your-hand).
tv_choice(opponent_hand) --> id(opponent-hand).

tv_wally(choice(your,pokemon(PokemonCategory))) --> id(choice), [colon], id(your-pokemon), pokemon_category(PokemonCategory).

pokemon_category(any) --> [].
pokemon_category(Cat) --> [colon], id(cat), [colon], pokemon_type(Cat).


tv_last(last_source) --> id(last), [colon], id(source).
tv_last(last) --> id(last).

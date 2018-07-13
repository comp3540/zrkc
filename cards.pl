:- module(cards, [cards//1, card//1,energy_type//1,trainer_type//1]).
:- use_module(lex).

cards_t([]) --> [].
cards_t(L) --> cards(L).

cards([H|T]) --> card(H), [nl], cards_t(T).

card(nil) --> [hash]. % Eat comments while keeping numbering
card(card(C)) --> pokemon_card(C).
card(card(C)) --> energy_card(C).
card(card(C)) --> trainer_card(C).
card(nil) --> []. % Eat blank lines while keeping numbering

energy_card(energy(Name,EnergyType)) --> [string(Name), colon, atom(energy),colon], energy_cat(EnergyType).

energy_cat(Energy) --> [atom(cat),colon], energy_type(Energy).
energy_type(psychic) --> [atom(psychic)].
energy_type(fighting) --> [atom(fighting)].
energy_type(fighting) --> [atom(fight)].
energy_type(lightning) --> [atom(lightning)].
energy_type(water) --> [atom(water)].
energy_type(colorless) --> [atom(colorless)].

trainer_card(trainer(Name,TrainerType,ability(Id))) --> [string(Name), colon, atom(trainer), colon, atom(cat),colon], trainer_type(TrainerType), [colon, int(Id)].
trainer_type(item) --> [atom(item)].
trainer_type(supporter) --> [atom(supporter)].
trainer_type(stadium) --> [atom(stadium)].

pokemon_card(pokemon(Name, PokemonType, evolves(EvFrom), energy(EnergyType), hp(InitialHP), RetreatCost, Attacks)) -->
	[string(Name),colon,atom(pokemon),colon], pokemon_evolution(PokemonType,EvFrom), [colon], energy_cat(EnergyType), [colon, int(InitialHP)], retreat_cost(RetreatCost),[colon], attacks(Attacks). 

pokemon_evolution(basic,nil) --> [atom(cat),colon,atom(basic)].
pokemon_evolution(stageone,EvFrom) --> [atom(cat),colon], id(stage-one), [colon,string(EvFrom)]. 

retreat_cost(retreat(nil,0)) --> [].
retreat_cost(retreat(EnergyType,NCards)) --> [colon,atom(retreat),colon], energy_cat(EnergyType), [colon, int(NCards)].

attacks(L) --> [atom(attacks),colon], attack_list(L).

attack_list_t([]) --> [].
attack_list_t(L) --> [comma], attack_list(L).
attack_list([H|T]) --> attack(H), attack_list_t(T).

%attack_list(LS) --> list(attack(L),comma,LS).

attack(attack(required(ReqList),ability(Id))) --> requirement_list(ReqList), [colon, int(Id)].

requirement_list_t([]) --> [].
requirement_list_t(L) --> [comma], requirement_list(L).
requirement_list([H|T]) --> requirement(H), requirement_list_t(T).

requirement(req(energy(EnergyType),NCards)) --> energy_cat(EnergyType), [colon,int(NCards)].

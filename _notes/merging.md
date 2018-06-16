Reorderable:
null
dam
heal
  * except as it relates to cond healed *
deenergize
  * except as it relates to searching discard *
reenergize
  * Not really
redamage
  * Amount of damage available depends on previous dam commands
swap
  * Not really
destat
  * Because you can't test it
applystat
  * Because you can't test it
draw
  * Affected by deenergize
  * Not reeorderable. Because of deck
search


Mergeable:
  applystat
Repeat:
  flip
Marked:
  dam, heal,draw, deck

By Opponent
  deck, draw, shuffle


cond(if(flip)

cond(if(flip(5))

applystat([])


dam(first,
dam(recurrence,

opponent_context(X) --> ['The opponent'], interpret_list(

healedDuringThisTurn flag


# Scratch:dam:target:opponent-active:20
board.opponent.active.hp -= 20
* if (hp == 0) eliminate

# Quick Attack:dam:target:opponent-active:10,cond:flip:dam:target:opponent-active:30

board.opponent.active.hp -= 10
if (rand) {
  board.opponent.active.hp -= 30
}

* if (hp === 0) eliminate

# Nuzzle:cond:flip:applystat:status:paralyzed:opponent-active

if (rand) {
  board.opponent.active.status += [paralayzed]
}

* making sure that paralysis prevents some stuff

# Circle Circuit:dam:target:opponent-active:20*count(target:your-bench)

board.opponent.active.hp -= 20 * length(board.your.bench)

GET LENGTH board.your.bench 
MATH A * 20
SET board.opponent.active.hp -MATH

# Thunderbolt:dam:target:opponent-active:100,deenergize:target:your-active:count(target:your-active:energy)

board.opponent.active.hp = -100
board.your.active.energy -= board.your.active.energy[0:board.your.active.energy.length]

energy water priority 10
energy colorless priority 0

* order of precedence of energy removals 
* attacks are enabled by an amount of energy


# Random Spark:dam:target:choice:opponent:30

$1 = select(1,filter([board.opponent.bench,board.opponent.active], any)
$1.hp -= 30

* damageable things

# Tierno:draw:3
your.hand.push(your.deck.pop(3))

# PokÃ©mon Center Lady:heal:target:choice:your:60,destat:target:last

$1 = select(1,filter([board.your.bench,board.your.active], any))
$1.hp += 60
$1.status -= [$1.status[0:n]]


* Resolve what last means in terms of a temporary variable

# Clemont:search:target:your:source:deck:filter:energy:4

$1 = ?search(4,filter([board.your.deck], c -> c.type == energy))
your.deck -= [$1]
your.hand.push($1)


# Ear Influence:redamage:source:choice:opponent:destination:opponent:count(target:last:source:damage)

* ?redamage is it's own kind of dialog
* Do the moves the user requested 

# Psychic:dam:target:opponent-active:60,dam:target:opponent-active:count(target:opponent-active:energy)*10

board.opponent.active.hp -= 60
board.opponent.avtive.hp -= length(board.opponent.active.energy) * 10

$1 = length(board.opponent.active.energy)
$2 = 10 * $1
board.opponent.active.hp -= $2

# Act Tough:dam:target:opponent-active:10,cond:count(target:your-active:energy:psychic)>0:dam:target:opponent-active:20

board.opponent.active.hp -= 10
$1 = length(filter([board.your.active.energy],(e -> e.type === psychic)))
if ($1 > 0) {
  board.opponent.active.hp -= 20
}

# Mine:search:target:opponent:source:deck:filter:top:1:0,cond:choice:shuffle:target:opponent

$1 = ?search(0,filter([board.opponent.deck[0:1]],any))
if (?agrees) {
  board.opponent.deck.shuffle()
}

# Scavenge:cond:ability:deenergize:target:your-active:1:(search:target:your:source:discard:filter:cat:item:1)

if (?argees) {
  board.your.active.energy.pop(1);
  $1 = ?search(1,filter([board.your.discard,(c -> c.type == item)))
  board.your.hand.push($1);
}


# Floral Crown:add:target:your:trigger:opponent:turn-end:(heal:target:self:20)

triggers.your.active.healed = fn

triggers.opponent.turnend = [() -> $self.hp += 20]
trigger.hpgoesto0



x = &y;
() -> x[1]++
x = &z;


# Shauna:deck:target:your:destination:deck:count(your-hand),shuffle:target:your,draw:5

$1 = board.your.hand.pop(length(board.your.hand))
board.your.deck.push($1)
board.your.deck.shuffle()
$2 = board.your.deck.pop(5);
board.your.hand.push($2)




======
dictionary.subdictionary.array
Maps, Arrays, Queues, Primitive Filters
x = value
+-/x, -=, +=
> < 
if, else
length
push
pop
shuffle
slice(0,n)
[ ] filter(lambda) lambdas x.attr === value
remove(i)
add(i,val)
User functions -- ask the user to make some choices
?agree - Do you want to x?
?select - Pick n cards from the set we gave you
?distribute - Move n things from set 1 we gave you to set 2 we gave you

Last, last source, self binding
Implicit choice (your means your active or bench for damage)


User events -- user should know that sth happened, doesnt make a decision
>flip
>set

Triggers

turn end
hp change trigger
energy change trigger
status trigger

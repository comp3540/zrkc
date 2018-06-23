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


# Red Card:deck:target:opponent:destination:deck:count(opponent-hand),shuffle:target:opponent,draw:opponent:4

$1 = length(board.opponent.hand)
$2 = board.opponent.hand.pop($1)
board.opponent.deck.push($2)
board.opponent.deck.shuffle();
$3 = board.opponent.deck.pop(4);
board.opponent.hand.push($3);


# Wally:search:target:choice:your-pokemon:cat:basic:source:deck:filter:evolves-from:target:last:1,shuffle:target:your

$1 = ?search(1,filter([board.your.bench,board.your.active]), (p -> p.cat = basic));
$2 = ?search(1,filter([board.your.deck],(p -> p.evolvesFrom = $1)));
$2.evolvedFrom = $1;
$1.parent.push($2);
$1.parent -= [$1];
board.your.deck.shuffle();

-------

$1 = filter([board.your.bench,board.your.active], (p -> p.cat = basic))
$2 = ?search(1,filter([board.your.deck],(p -> filter([$1], (q -> p.evolvesFrom = q)))) 

* Probably better if there was one search dialog with a more advanced filter

# Shauna:deck:target:your:destination:deck:count(your-hand),shuffle:target:your,draw:5

$1 = board.your.hand.pop(length(board.your.hand))
board.your.deck.push($1)
board.your.deck.shuffle()
$2 = board.your.deck.pop(5);
board.your.hand.push($2)


# Sleep Poison:cond:flip:(applystat:status:asleep:opponent-active,applystat:status:poisoned:opponent-active)

if (rand()) {
  board.oppnent.active.status += [asleep,poisoned]
}

# Misty's Determination:cond:ability:deck:target:your:destination:discard:choice:you:1:(search:target:your:source:deck:filter:top:8:1,shuffle:target:your)

if (?agrees()) {
  $1 = ?select(1,filter([board.your.hand],any));
  board.your.hand -= [$1];
  board.your.discard.push($1);
  $2 = ?select(1,filter([board.your.deck[1:8]],any));
  board.your.hand.push($2)
  board.your.deck.shuffle();
}





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

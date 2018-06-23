## Card Arrays

Card arrays are used to represent the different places in the game board that a card may be in. Card arrays hold card objects, and have certain basic operations.

board 
  opponent
    deck [O]
    discard [O]
    bench
    hand
    active
    prize [O]
  your
    "

All collections are unordered unless marked [O]

## Card Objects

Must hold the reference properties (like name, initial hp, etc.), but also track the current state of a card in the game. State variables may have associated triggers, which must run each time the state variable is changed.

  ### hp
    Triggers the hp change hook
  
  ### energy [O]
    Is an array of associated energy types. Energy cards are inserted according to a specific priority.

  ### status
    Is a set of statuses
    Triggers the status change hook

  ### evolvedFrom
    Is used to cause the UI to render the basic card this stage one evolved form, if applicable.
    Triggers the evolution hook

  ### attachedItems
    Is an array of attached item cards

  ### attachedTo
    The pokemon you are attached to

## Basic Operations

  pop(n)          - [top/bottom]
  push(items)     - [top/bottom]
  get(i)          - access a specific item
  set(i,value)
  slice(lo,hi)    - only a subset of the array
  shuffle()
  remove(i)       - Delete item at i
  filter(arrays, lambda)
  contains(val)   - Some card exists which matches val
  len()           - Get length of this array

## Filter lambdas
  Filter lambdas must consist only of checks as to whether card properties or state match what we're looking for. e.g.
      (card -> card.type = stadium)

## Triggers
  A trigger will also be fired at
    your/opponent turn start/end

It is possible to add trigger lambdas to any event. They can use the attachedTo reference to find out which card that item card is controlling. When the attachedTo card is removed from play, the trigger must be deleted.
  
Most of the rules surrounding elimination of cards are implemented on a "hp" trigger, if hp == 0. cond:healed is another hp trigger: hpFinal > hpInitial.
  
Triggers could also be used to clear special conditions at turn end.

## Blocks

We support only an if.
if (test) {
  code
} else {
  code
}

### Tests
  You can do comparisons on any basic operation (board.your.hand.length() > 2), but there are some special boolean functions which can be used as tests.
  rand() - 50% chance of being true
  ?agrees(question) - User agrees to the question you asked

## Special functions

?select(n,array) - User can pick n cards from the subset we specified
?distribute(n,a,b) - User can move n cards from the specified subset into the specified destination (might be hard - redamage)

## Temporary variables

Each time an operation acts on a specific card, or on a set of cards, it is pushed onto the temp stack. Arguments like last and last source will pop the temp stack to figure out what card they refer to. For ease of notation, entries in the temp stack are referred to by $1...$n.

It's also possible to need temporary variables to describe operations that can't be done in place, but don't go on the temp stack. These variables can be \_1 through \_n.

## Target languages

- JS supports all of the features we need
- Prolog can be tricked into having arrays and lambdas
- Java 8 can do this, so we can explain it in terms of this language
- Python has the basic data structures
- Pretty sure OCaml does, and so would *modern* LISPs.


## Error handling

If we pop the temp stack and find the wrong kind of card or set of cards, we raise a runtime error. If for some reason you're asked to search an empty set of cards, we raise a runtime error. 

**It must be impossible to pop a card without pushing it somewhere else.** To that end, it might be better for our basic operations to be more like:
  move(n,a,b) - Move top/bottom n cards from a to b
  relocate(card,a,b) - Remove card from a and push it onto b

We also need to prevent setting values of the wrong type (e.g. hp cannot be -20, energy cannot hold a trainer card, you can't evolve from the string "duck")

We will not prevent users from selecting abilities that don't make sense or are not beneficial.
"Either the ability you've tried to play is invalid, or you've failed to verify whether you may use this ability given the current state of play.
Specifically, can't take 3 cards from a collection of 0"
(this is a temporary notification bar, not a modal).

## How do we build the code generation module

1. First we need to transform our ability AST, into an intermediate representation [IR], which looks like the code examples from earlier but are in Prolog notation.
2. Secondly, we can send these IRs to either Prolog or JS to interpret and actually do what we've specified.
3. Alternatively, we can transform these IRs into actual Prolog or JS code.
  * For Prolog, this is just another tree transformation from our IR to actual Prolog predicate structures
  * For JS, it's kind of like our English interpretations were we emit strings to match some expected form.
    - Technically it's possible to do the AST based technique with JS too, but that's harder.

# What features need to be done in native code

1. Enforcing special conditions
2. User moves cards around without using an ability
3. Whose turn it is
4. The ?agrees, ?select, and ?distribute dialogs


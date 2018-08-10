$A,$B,... are values on the stack


Numbers
--------------
ADD
SUB
MUL
DIV


Objects
-----------
LVAL fieldname 
  $A.fieldname
SVAL fieldname 
  $A.fieldname = $B

Loading Lists
--------------
LTARGET y/active
PEEKTARGET (Places the last target ontop of the stack)
LSOURCE o/deck
PEEKSOURCE (Places the last source ontop of the stack)
LTEMP y/bench

These basically do stack.push(board.opponenrt.discard), etc,
but also do some record-keeping for abilities that use last.

Manipulating lists
------------------
PUSH  
  $A.push($B)
BPUSH (Bottom of the list)
POP
   $A.pop()
BPOP (Bottom of the list)
CONCAT 
  $A.concat($B)
BCONCAT 
  $B.concat($A)
REMOVE (Does A \ B in terms of set theory)
SLICE lo hi 
  $A.slice(lo,hi) [non-destructive]
LENGTH 
  $A.length
SHUF
  $A.shuffle()

User interface
-------------
SELECT 
  Dialog allowing you to select $B cards from list $A 
CONFIRM message
  Dialog asking you to say yes or no to something.

CALL arbitrary JS code
  For like wally or floral crown or sth.

Controlling the VM
-----------------
JZ to (Jump if some value is zero) 
  if (!$A) { pc = to }
CONST val (Used to load an arbitrary constant)
  stack.push(val)
# comment (Just for logging)


Filters (for searches not counts)
--------------------------------
[non destructive]

FILTCLASS class
  $A.filter(ai => ai instanceof class)

FILTYPE type
  $A.filter(ai => ai.type == type)

FILTSTAGE stage
  $A.filter(ai => ai.stage == stage)


Examples
-------

Misty's Determination:cond:ability:deck:target:your:destination:discard:choice:you:1:(search:target:your:source:deck:filter:top:8:1,shuffle:target:your)


ability(name("Misty's Determination"),
	[ cond(if(you_do([ deck(your,
				destination(discard,top),
				choice(your),
				1)
			 ])),
	       then([ search(your,
			     source(deck),
			     filter(placement(top,8)),
			     1),
		      shuffle(your)
		    ]),
	       else(null))
	])

If you choose to use this ability place 1 card from your hand onto your discard pile; look through the top 8 cards of your deck, then take 1 card; shuffle your deck.



0000    CONFIRM "Do you want to..."       # If you choose...
0001    JZ 13                             # Get out if not
0002    LSOURCE y/hand                    # "from your hand"
0003    CONST 1
0004    POP                               # 1 card
0005    LTARGET y/discard                 # "onto your discard pile"
0006    CONST 1
0007    PUSH                              # Move the card
0008    LSOURCE y/deck                    # "of your deck"
0009    SLICE 0 7                         # "the top 8"
0010    CONST 1                           # "take 1 card"
0011    LTARGET y/hand                    # The cards will go into your hand
0012    CONCAT                            # Put them on the hand
0013    LTARGET y/deck                    # "your deck"
0014    SHUF                              # "shuffle"
0015    NOP                               # No operation


--------------------


Act Tough:dam:target:opponent-active:10,cond:count(target:your-active:energy:psychic)>0:dam:target:opponent-active:20


0000  LTARGET o/active          
0001  LVAL    hp
0002  CONST   10
0003  SUB
0004  SVAL hp
0005  LTARGET y/active
0006  LVAL energy       ;; It's ok that energy is an array here
0007  FILTYPE psychic
0008  LEN
0009  CONST 0
0010  SUB
0011  JZ 17
0012  LTARGET o/active
0013  LVAL  hp
0014  CONST 20
0015  SUB
0016  SVAL hp
0017  NOP



Shauna:deck:target:your:destination:deck:count(your-hand),shuffle:target:your,draw:5

0000  LTARGET y/hand
0001  LEN
0002  LSOURCE y/hand
0003  POP
0004  LTARGET y/deck
0005  CONCAT
0006  LTARGET y/deck
0007  SHUF
0008  LSOURCE y/deck
0009  CONST 5
0010  POP
0011  LTARGET y/hand
0012  CONCAT


## Issues to resolve
Do we need copy switch, etc, could this be a mess
Concat vs push
How do we handle choice?
As a select 1? It's a useless list
PUSH
POP

Why slice vs pop?
Destructive vs not?

DUP
CONST 8
POP


* Choice could just be a select 1
and then POP
To get rid of it

How do we see the original

Targets and sources are they 

They're individual cards

So I'd just do
LLIST y/active
POPTARGET

LLIST y/bench
SLICE 0 5
CONST 1
SELECT
POPTARGET


#####
long names

div
mul
sub
add
load-val
store-val
load-cards
const
mark-as-target
mark-as-source
mark-as-holding
pop-from-bottom
pop
last-target
last-source
push
push-to-bottom
concat
concat-inverse
remove-from-list
slice [Non-destructive]
shuffle
rand
filter-by-class [Non-destructive]
filter-by-type [Non-destructive]
filter-by-stage [Non-destructive]
filter-by-evolved-from [Non-destructive]
else-goto
length
random
confirm
select


## Damage opponent active 20
load-cards o/active
const 1
pop
mark-as-target
load-value hp
const 20
sub
store-value hp

Maybe we load code down to individual blocks
A block is aborted if any instruction within it fails?


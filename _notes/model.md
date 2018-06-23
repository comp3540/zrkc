board:
  opponent
  your
    deck
    discard
    hand
    bench
    active

Card:
  type
  hp
  energy
  status
  item
  evolution

SET hp 20
  * HP is 0, remove card
SET energy [1 water]
SET status
SET evolution

MOVE x FROM deck TO hand
SELECT card WHERE energy > 0
LENGTH
ARITH

IF with inequalities

FILTER
  for e in array:
    if fn(e) == true:
      result += e
    else:
      # do nothing


move(cardsToMove,destinationArray,i)
attach
detach

?YESNO cond:choice
?SELECT n card, either from some array, or from some array and with some criteria, 
?FLIP 
?

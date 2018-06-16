
## Consider this yocode

```prolog

yo(Val,[yo:Val|Slot], Slot).

?- foldl(yo,[1,2,3,4],Result,[]).
```

Let's write this code in a more verbose form:
```prolog
yo(Val,CurrentSlot,NewSlot) :-
  CurrentSlot = [yo:Val|NewSlot].

?- foldl(yo,[1,2,3,4],Result,FinalSlot), FinalSlot = [].
```

What we are saying is that we know how to calculate some value, but we do not know what comes after it,
therefore, we place this value in the list, and return `Slot,` an unbound variable, where the next calculated
value should be inserted.

At the end of the fold we're given back the accumulator. What's left at the end in the accumulator is one last
unused slot (what comes after `4`?). We put `[]` in the last slot, stopping the list.
When we called fold, `Result` was unbound -- it was a slot. `1` immediately used this slot, so `Result` is now a pointer
to the head of the list.

You could use this sort of technique in any programming language. You could imagine a pointer in C called `writeHere`
which indicates where a function could put a value in order for it to come next in a linked list.
If you were doing this iteratively instead, writeHere would just be `i`. Sometimes you increment `i` when you write a new
value in the linked list, or you can leave `i` unchanged if you didn't put anything there.

:- use_module(library(clpfd)).

solve(true) :- !.
solve((A,B)) :- !, solve(A), solve(B).
solve(H) :- clause(H,B), solve(B).

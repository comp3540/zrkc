:- module(arith,[arith//1,ineq//1]).
:- multifile count_expr//1.
:- use_module(lex).

arith(Expr) --> terms(Expr).

ineq_op(<) --> [lt].
ineq_op(=<) --> [leq].
ineq_op(>) --> [gt].
ineq_op(>=) --> [geq].
ineq_op(=) --> [eq].
ineq_op(=:=) --> [neq].

ineq(Struct) --> arith(ExprA),ineq_op(Op),!, arith(ExprB), {Struct =.. [Op,ExprA,ExprB]}.

terms_t(FactorA, Terms) --> [plus], factors(FactorB), ! , terms_t(FactorA + FactorB, Terms).
terms_t(FactorA, Terms) --> [-], factors(FactorB), !, terms_t(FactorA - FactorB, Terms).
terms_t(Terms, Terms) --> []. % We are done when there are no further additions or subtractions
terms(Terms) --> factors(Factor), terms_t(Factor, Terms).

factors_t(ValueA, Factors) --> [mult], value(ValueB), !, factors_t(ValueA * ValueB, Factors).
factors_t(ValueA, Factors) --> [div], value(ValueB), ! ,factors_t(ValueA / ValueB, Factors).
factors_t(Factors,Factors) --> []. % We know we are done when there are no further multiplications or divisions
factors(Factors) --> value(Value), factors_t(Value, Factors).

value(Value) --> id(count), [lparen], count_expr(Value), [rparen].
value(Value) --> [lparen], arith(Value), [rparen].
value(Value) --> [int(Value)].


:- module(lex,[lex/2,id//1]).
:- use_module(library(pcre)).

lex_symbol(":",":",colon).
lex_symbol(",",",",comma).
lex_symbol("\n","\n",nl).
lex_symbol("\\(","(",lparen).
lex_symbol("\\)",")",rparen).
lex_symbol("\\*","*",mult).
lex_symbol("/","/",div).
lex_symbol("-","-",-). % Because reasons
lex_symbol("\\+","+",plus).
lex_symbol("<","<",lt).
lex_symbol("<=","<=",leq).
lex_symbol("=","=",eq).
lex_symbol(">=",">=",geq).
lex_symbol(">",">",gt).
lex_symbol("!=","!=",neq).


lex_value(int,"^(0|[1-9][0-9]*)$", number_string).
lex_value(atom,"^[a-z-]+$", atom_string).
lex_value(string,"^[A-za-z '-é]+$", identity).

identity(X,X).

symbol_regex(Re) :-
	findall(Symbol, lex_symbol(Symbol,_,_),SymbolS),
	atomics_to_string(SymbolS,"|",Alts0),
	string_concat("(",Alts0,Alts1),
	string_concat(Alts1,")",Re).


lex(String,Tokens) :-
	symbol_regex(SplitRe),
	re_split(SplitRe,String,Bits0),
	subtract(Bits0,[""],Bits),
	maplist(tokenize,Bits,Tokens), !.

tokenize(Symbol,Token) :- lex_symbol(_,Symbol,Token).

tokenize(Value,ValueToken) :-
	lex_value(Const,Re,Converter),
	re_match(Re,Value),
	call(Converter,Result,Value),
	ValueToken =.. [Const,Result].

id(A) --> [atom(A)].
id(A-B) --> [atom(A),-,atom(B)].

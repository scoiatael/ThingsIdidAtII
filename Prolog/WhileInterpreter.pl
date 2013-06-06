%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A Prolog implementation of the natural semantics 
%% for the While language (arithmetic expressions, really).                            


%%%%%%%%%%%%%%%%%%%%%
%% State operations

% lookup_state(State, Variable, Value)

lookup_state([(X,V)|_], X, V).
lookup_state([(Y,_)|S], X, V) :-
	Y \= X,
	lookup_state(S, X, V).

% update_state(State, Variable, Value, NewState)

update_state([], X, V, [(X,V)]).
update_state([(X,_)|S], X, V, [(X,V)|S]).
update_state([(Y,U)|S], X, V, [(Y,U)|S1]) :-
	Y \= X,
	update_state(S, X, V, S1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Natural semantics of arithmetic expressions

% aexp_ns(Aexp, State, Number)

aexp_ns(num(N), _, N).

aexp_ns(var(X), S, N) :-
	lookup_state(S, X, N).

aexp_ns(add(A1,A2), S, N) :-
	aexp_ns(A1, S, N1),
	aexp_ns(A2, S, N2),
	N is N1 + N2.

aexp_ns(sub(A1,A2), S, N) :-
	aexp_ns(A1, S, N1),
	aexp_ns(A2, S, N2),
	N is N1 - N2.

aexp_ns(mul(A1,A2), S, N) :-
	aexp_ns(A1, S, N1),
	aexp_ns(A2, S, N2),
	N is N1 * N2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Bexp

% bexp_ns(Bexp,State,Value)

bexp_ns(true,_,tt).
bexp_ns(false,_,ff).

bexp_ns(and(B1,B2),S,tt):- bexp_ns(B1,S,V1), bexp_ns(B2,S,V2), V1==tt, V2==tt,!.
bexp_ns(and(_,_),_,ff).
bexp_ns(or(B1,B2),S,tt):- bexp_ns(B1,S,V1), bexp_ns(B2,S,V2), (V1==tt; V2==tt),!.
bexp_ns(or(_,_),_,ff).
bexp_ns(no(B),S,tt):- bexp_ns(B,S,V), V==ff,!.
bexp_ns(no(_),_,ff).

bexp_ns(mn(A1,A2),S,tt):- aexp_ns(A1,S,V1), aexp_ns(A2,S,V2), V1<V2,!.
bexp_ns(mn(_,_),_,ff).

bexp_ns(row(A1,A2),S,tt):- aexp_ns(A1,S,V1), aexp_ns(A2,S,V2), V1==V2,!.
bexp_ns(row(_,_),_,ff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Instrukcje

% $(Stm, State, State)

$(podstaw(X,A),S1,S2):- aexp_ns(A,S1,V), update_state(S1,X,V,S2).
$(zloz(Stm1,Stm2),S1,S2):- $(Stm1,S1,Sp), $(Stm2,Sp,S2).

$(skip,S,S).

$(if(B,Stm1,_),S1,S2):-bexp_ns(B,S1,V), V==tt, !, $(Stm1,S1,S2).
$(if(_,_,Stm2),S1,S2):- $(Stm2,S1,S2).

$(while(B,_),S,S):-bexp_ns(B,S,V), V==ff,!.
$(while(B,Stm),S1,S2):-bexp_ns(B,S1,V), V==tt, $(Stm,S1,Sp), $(while(B,Stm),Sp,S2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Adding some syntactic sugar

% State

:- op(80, xfx, @).
(S,V) @ X :-
	lookup_state(S, X, V).

% Aexp

:- op(120, yfx, :+:).
:- op(120, yfx, :-:).
:- op(100, yfx, :*:).

% Semantics

:- op(180, xfx, --->).

(N, S) ---> N :-
	number(N).

(X, S) ---> N :-
	atom(X),
	(S,N) @ X.

(A1 :+: A2, S) ---> N :-
        (A1, S) ---> N1,
	(A2, S) ---> N2,
	N is N1 + N2.

(A1 :-: A2, S) ---> N :-
        (A1, S) ---> N1,
	(A2, S) ---> N2,
	N is N1 - N2.

(A1 :*: A2, S) ---> N :-
        (A1, S) ---> N1,
	(A2, S) ---> N2,
	N is N1 * N2.
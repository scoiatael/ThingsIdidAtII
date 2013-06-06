select(H, [H|T],T).
select(X, [H|T],[H|Z]):-select(X,T,Z).

permw([],[]).
permw([H|T], Z):- permw(T,Y), select(H,Z,Y).

filter([],[]).
filter([H|T],[H|Z]):-H>=0, filter(T,Z).
filter([H|T],Z):-H<0, filter(T,Z).

count(_, [], 0).
count(H, [H|T], X) :- count(H,T,X0), X is X0+1.
count(H, [H1|T], X) :- count(H,T,X), H1\=H.

count2(_, [], 0).
count2(H, [H|T], X) :- count2(H,T,X0), X is X0+1, !.
count2(H, [H1|T], X) :- count2(H,T,X).


exp(_, 0, 1):-!.
exp(X, A, Z) :- A0 is A-1, exp(X,A0,Z0), Z is Z0*X.

factorial(1, 1):-!.
factorial(X, Z) :- X>1, X0 is X-1, factorial(X0, Z0), Z is Z0*X.

factorial2(1,A,A):-!.
factorial2(X, Z,D) :-X0 is X-1, D1 is D*X, factorial2(X0, Z, D1).
factorial2(A,B):-factorial2(A,B,1).


concat_number(Dig, Num) :- reverse(Dig,Dig1), concat_numbera(Dig1,Num).
concat_numbera([H], H):-!.
concat_numbera([H|T], N):- concat_numbera(T, N0), N is N0*10+H.

decimal(N, X):- decimala(N, X0), reverse(X0,X), !.
decimala(X, [X]):-!.
decimala(N, [H|T]):- H is N mod 10, N0 is N//10, decimala(N0, T).

select_min([H], H, []).
select_min([H|T], X, [H|P]):- select_min(T,X,P), H>=X.
select_min([H|T], H, T):- select_min(T, X, _), H<X.

sel_sort([], []).
sel_sort([H|T], [X|Z]):-select_min([H|T], X, P), sel_sort(P, Z).
:- use_module(library(arithmetic)).
:-arithmetic_function(!/1).
:-op(200, xf,!).
!(X,W):- !(X,1,W).
!(0,A,A):- !.
!(X,Y,Z):- Y0 is Y*X, X0 is X-1,!(X0,Y0,Z).

:-arithmetic_function('!!'/1).
:-op(200, xf,'!!').
'!!'(X,W):- X mod 2 =:= 0,!, X0 is X-1, '!!'(X0, 1, W).
'!!'(X,W):- '!!'(X, 1, W).
'!!'(1,A,A):- !.
'!!'(X,Y,Z):- Y0 is Y*X, X0 is X-2,'!!'(X0,Y0,Z).


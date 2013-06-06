mlength(X,Y):-mlength(X,Y,0).
mlength([], A,A).
mlength([_|T], B, X):- X\=B, X1 is X+1, mlength(T,B,X1).

trip(X,Y,[X|Z]):-trip(X,Y,Z,[]).
trip(X,X,[],_).
trip(X,Y,[H|T],Z):-connection(X,H), \+(member(H,Z)), trip(H,Y,T,[H|Z]).

connection(gliwice,wroclaw).
connection(wroclaw,warszawa).
connection(wroclaw, katowice).
connection(katowice, warszawa).
connection(katowice,gliwice).
connection(wroclaw,gliwice).
connection(katowice,wroclaw).

bin([X]):-X=0.
bin([H|T]):-H=1, binc(T).
binc([]).
binc([H|T]):-binc(T),or(H).
or(X):-X=0.
or(X):-X=1.

rbin([H]):-H=0.
rbin([H|T]):-rbinc(T), or(H).
rbinc([H]):-H=1.
rbinc([H|T]):-rbinc(T), or(H).

mirror(leaf,leaf).
mirror(node(X,C,Y), node(Y1,C,X1)):-mirror(Y,Y1), mirror(X,X1). 

flatten(X,Y):-flatten(X,Y,[]).
flatten(leaf,A,A).
flatten(node(X,C,Y), T, XT):-flatten(Y,T1,XT), flatten(X,T,[C|T1]).

insert(X, leaf,Y):-Y=node(leaf,X,leaf).
insert(E, node(X,C,Y),T):- E<C, insert(E, X, X1),T=node(X1,C,Y).
insert(E, node(X,C,Y),T):- E>=C, insert(E, Y, Y1),T=node(X,C,Y1).

binsort(X,Y):-drz(X,T),flatten(T,Y).
drz([], leaf).
drz([H|T], L):-drz(T,L1), insert(H,L1,L).

lamiglowka([A, C, E, P, R, S, U]):-sublist([A1, C1, R1, S1],[2,3,4,5,6,7,8]), lamiglowka([A, C, E, P, R, S, U], [A1, C1, R1, S1]). 
lamiglowka([A, C, E, P, R, S, U], Lista):-permutation(Lista, Z), Z=[A, C, R, S], E=0 ,U=9, P=1, concat_number([U,S,A], Sk1), concat_number([U,S,S,R], Skl2), concat_number([P,E,A,C,E], Sum), Sum is Sk1+Skl2.


concat_number(Dig, Num) :-concat_numbera(Dig,Num,0).
concat_numbera([], H, H):-!.
concat_numbera([H|T], N, N1):- N0 is N1*10+H, concat_numbera(T, N, N0).

sublist([],_).
sublist([H1|T1], [H1|T2]):-sublist(T1,T2).
sublist([H1|T1], [_|T2]):-sublist([H1|T1], T2).

revall(X,Y):-length(X,XL), length(Y,YL), XL=:=YL, !,revall(X,Y,[]).
revall([],Y,Y).
revall([H|T],T1, T2):-H=[_|_], !, revall(H,H1), revall(T,T1, [H1|T2]).
revall([H|T],T1, T2):-revall(T,T1,[H|T2]).
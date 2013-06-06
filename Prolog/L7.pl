%1:

st_put(E,S,[E|S]).
st_get([E|S],E,S).
st_empty([]).
st_addall(E,G,S,R):-findall(E,G,R1),append(R1,S,R).

qu_put(E,H-[E|C],H-C).
qu_get([E|H]-T,E,H-T).
qu_empty(C-C).
qu_addall(E,G,H-T,H-C):-findall(E,G,R1), append(R1,C,T).

%2


dfs(X,Y):-dfs([X],[],Y,[]).
dfs([],_,_).
dfs([H|_],_,H,Path):-reverse([H|Path],P2),print(P2).
dfs([H|T],Odw,H,[_|P]):-dfs(T,Odw,H,P).
dfs([H|T],Odw,Y,[_|P]):-member(H,Odw),!,dfs(T,Odw,Y,P).
dfs([H|T],Odw,Y,Path):-H\=Y,st_addall(X,e(H,X),T,T1),dfs(T1,[H|Odw],Y,[H|Path]).


bfs(X,Y):-bfs([X|C]-C,[],Y).
bfs([H|_]-_,_,H).
bfs([H|T]-T1,Odw,Y):-nonvar(H),member(H,Odw),!,bfs(T-T1,Odw,Y).
bfs([H|T]-T1,Odw,Y):-nonvar(H),!,qu_addall(X,e(H,X),T-T1,Tp-T1p), bfs(Tp-T1p,[H|Odw],Y).
bfs(C-C,_,_):-!.

bfs2(X,Y):-bfs2([X|C]-C,[],Y).
bfs2([H|_]-_,Odw,H):-print(Odw),nl.
bfs2([H|T]-T1,Odw,Y):-nonvar(H),member(H,Odw),!,bfs2(T-T1,Odw,Y).
bfs2([H|T]-T1,Odw,Y):-nonvar(H),!,qu_addall(X,e(H,X),T-T1,Tp-T1p), sortm(Tp-T1p,Tpp-T1pp), bfs2(Tpp-T1pp,[H|Odw],Y).
bfs2(C-C,_,_):-!.

sortm(Tp-[],Tpp-T1pp):-sort(Tp,Tpx),append(Tpx,T1pp,Tpp).

e(4,2).
e(4,3).
e(3,1).
e(2,5).
e(5,1).

%3:

insert(X,leaf,node(leaf,X,leaf)).
insert(X, node(A,B,C), node(A1,B,C)):-X<B,insert(X,A,A1).
insert(X, node(A,B,C), node(A,B,C1)):-X>B,insert(X,C,C1).

find(X, node(_,X,_)):-!.
find(X, node(A,_,C)):- find(X,A);find(X,C).

max(A,B,A):-A>B,!.
max(_,B,B).

%findmax(?Max, +Tree).
findmax(X, node(leaf,X,leaf)).
findmax(Y, node(A,X,leaf)):-findmax(Z,A), max(Z,X,Y).
findmax(Y, node(leaf,X,A)):-findmax(Z,A), max(Z,X,Y).
findmax(Y, node(A,X,C)):-findmax(Z,A), max(Z,X,Y1), findmax(W,C), max(W,Y1,Y).

%delmax(?Max, +TreeIn, -TreeOut)
delmax(X, node(A,X,leaf),A).
delmax(X, node(A,B,Z), node(A,B,Z1)):-delmax(X,Z,Z1).

%delete(+What, +TreeIn, ?TreeOut)
delete(X,node(A,X,leaf), A):-!.
delete(X,node(leaf,X,B),B):-!.
delete(X,node(A,X,B), P):-!,sum(A,B,P).

delete(X, node(A,Y,B), node(A1,Y,B)):-X<Y,!, delete(X,A,A1).
delete(X, node(A,Y,B), node(A,Y,B1)):-delete(X,B,B1).

%sum(+TreeSmaller,+TreeBigger, -TreeOut)
sum(node(A,B,leaf),C, node(A,B,C)):-!.
sum(A,node(leaf,B,C), node(A,B,C)):-!.
sum(node(A,B,C), D, node(A,B,X)):-sum(C,D,X).

empty(leaf).

%4:

ukonkr(c,8,_).
ukonkr(s,9,_).
ukonkr(P,X,Ak):-Ak<4, Ak0 is Ak+1, ukonkr(P,X0,Ak0), X is X0-2.

ukonkr([],[]).
ukonkr([H|T], [H1|T1]):-!,ukonkr(H,H1), ukonkr(T,T1).
ukonkr(X,X1):-ukonkr(X,X1,0).

concat_number(Dig, Num):- reverse(Dig,Dig1), (Dig1=[Num];concat_numbera(Dig1,Num)).

concat_numbera([H], H):- H\==0.
concat_numbera([H|T], N):- concat_numbera(T, N0), N is N0*10+H.

decimal(N, X):- decimala(N, X0), reverse(X0,X), !.
decimala(X, [X]):-X<10,!.
decimala(N, [H|T]):- H is N mod 10, N0 is N//10, decimala(N0, T).

iter([],[]).
iter([H|T], [H1|T1]):-concat_number(H,H1), iter(T,T1).

select(H,[H|T],T).

pomn(Li,[H|T],[H1|T1]):-!,pomn(Li,H,H1), pomn(Li,T,T1).
pomn(_,[],[]):-!.
pomn(Li,Cy,W):-W is Li*Cy.

domn(T,T,G,G):-!.
domn([H|T],[H1|T1],G,N):-H1 is H*(10**N), N0 is N+1, domn(T,T1,G,N0).

sum([], H, H):-!.
sum([H|T], Ak, L):-Ak0 is Ak+H, sum(T,Ak0, L).

check(R,[],R):-!.
check([H|T],[X|Y],R):-decimal(X,X1), ukonkr(H,X1),check(T,Y,R).

solve2(X,[Pi,Dr,LOst]):-select(Pi1,X,Rp), select(Dr1,Rp,R), ukonkr(Pi1,Pi), ukonkr(Dr1,Dr), concat_number(Pi,LPi), pomn(LPi,Dr, Wy),check(R,Wy,[Ost]),length(Dr,G), domn(Wy,W,G,0),sum(W,0, LOst),check([Ost],[LOst],_).

solve(X,[Pi,Dr,LOst]):-select(Pi1,X,Rp), select(Dr1,Rp,R), ukonkr(Pi1,Pi), ukonkr(Dr1,Dr), concat_number(Pi,LPi), concat_number(Dr,LDr), pomn(LPi,Dr, Wy),check(R,Wy,[Ost]),LOst is LPi*LDr,check([Ost],[LOst],_).

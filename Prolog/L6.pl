flattenx([],C-C):-!.
flattenx([H|T1], Z-C):-!, flattenx(H,Z-X), flattenx(T1,X-C).
flattenx( X, [X|C]-C).

flatten1(X,Y):-flattenx(X,Y-[]).

halve(X,L,R):-halve(X,L,R,X).
halve([H|Y1], [H|L], R, [_,_|Y2]):-!,halve(Y1,L,R,Y2).
halve(X,[],X,[_]).
halve(X,[],X,[]).

merge([],X,X):-!.
merge(X,[],X):-!.
merge([H1|Y1], [H2|Y2], [H1|Z]):-H1<H2,!,merge(Y1, [H2|Y2],Z).
merge([H1|Y1], [H2|Y2], [H2|Z]):-merge([H1|Y1], Y2,Z).

merge_sort([],[]):-!.
merge_sort([H],[H]):-!.
merge_sort(X,Y):-halve(X,L,R), merge_sort(L,L1), merge_sort(R,R1), merge(L1,R1,Y).

halve2(X-Y,L,R):-halve2(X-Y, L, R, X-Y).
halve2(X-Y, X-X, X-Y, Y-Y).
halve2(X-Y,X-X,X-Y,[_|Y]-Y).
halve2(X-Y, X-Z, W-Y, [_,_|Y2]-Y):-!,halve2(X-Y,X-[H|Z],[H|W]-Y,Y2-Y).

merge_sort2(X-X,[]):-!.
merge_sort2([H|X]-X,[H]):-!.
merge_sort2(X-T,Y):-halve2(X-T,L,R), merge_sort2(L,L1), merge_sort2(R,R1), merge(L1,R1,Y).

split([], _,[], []).
split([H|T], M, [H|S], B):-H=<M, split(T,M,S,B).
split([H|T], M, S, [H|B]):-H>M, split(T,M,S,B).

qsort(A,B):-qsort(A,B,[]).
qsort([],A,A).
qsort([H|T],W,Ak):-split(T,H,S,B), qsort(B,X,Ak), qsort(S,W,[H|X]).

sum_mine(X,Y,Z):-gen(X), gen(Y,X), gen(Z,X), Z is X+Y.

gen(X):-nonvar(X).
gen(X):-var(X), gen(X,0).
gen(X,X).
gen(X,Y):-Y1 is Y*(-1)+1, gen2(X,Y1).
gen2(X,X).
gen2(X,Y):-Y1 is Y*(-1), gen(X,Y1).

prime(X):-nonvar(X), prime1(X).
prime(X):-var(X), prime1(X,0).
prime2(X):-var(X), gen_sito(P-[],1000), member(X,P).

prime1(X):- !, X0 is X, gen_sito(P-[],X0), member(X0, P),!.
prime1(X,A):- (prime(A), X=A);(A1 is A+1, prime1(X,A1)).
gen_sito(X-T,Ak):- Ak>=2, Ak0 is Ak-1, gen_sito(X-T0,Ak0), ((\+ check_pr(Ak, X-T0),!,T0=[Ak|T]);(true, T0=T)).
gen_sito(C-C,1). 

member1(H, [H|[]]-[]).
member1(H, Z-[]):-Z\=T, Z=[A|B], (H=:=A; member1(H,B-T)).

check_pr(P,[H|T]-[]):-0 is P mod H,!.
check_pr(P,[_|T]-[]):- check_pr(P,T-[]).

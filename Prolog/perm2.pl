my_length([],0).
my_length([_|T],Z):- my_length(T, Z0), Z is Z0+1.

select(H, [H | T], T).
select(X, [H | P], [H | T]) :- select(X, P, T).

permd([], []).
permd([H1 | T1], [H2 | T2]) :- permd(P2, T2), select(H2, [H1 | T1], P2).

perm(A,B):- my_length(A,AL), my_length(B,BL), AL=:=BL, !, permd(A,B).
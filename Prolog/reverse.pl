reverse(X,Y) :- length(X, XL), length(Y, YL), XL=:=YL, !,reverse(X,[],Y).

reverse([],A,A).
reverse([H|T],A,Y) :- reverse(T,[H|A],Y).
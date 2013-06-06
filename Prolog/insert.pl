insert(X, [], [X]).
insert(X, [H | T], [H | P]) :- X>=H, insert(X, T, P).
insert(X, [H | T], [X | [H | T]]) :- X<H.

insert_sort([],[]).
insert_sort([H|T], X):-insert_sort(T, Z), insert(H, Z, X).
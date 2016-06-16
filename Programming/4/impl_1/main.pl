%% Ogolny opis algorytmu znajduje sie przy predykacie main (l 137)

%% Obsluga wybranej reprezentacji danych - jako tablicy zmiennych i atomow (liczb, 'w', 'c')
%Generowanie tablicy 2 wymiarowej o wymiarach m x n, wypelnionej atomami 'wolne':
gen([],         _,0):-!.
gen([Head|Tail],M,N):- genRzad(Head,M),N0 is N-1, gen(Tail,M,N0).

genRzad([])       :-!.
genRzad([wolne|T]):-genRzad(T).

genRzad(L,M):- length(L,M), genRzad(L).

%Sprawdzenie czy element tablicy o wspolrzednych (M,N) jest zajety przez element El:
zajety((_,_,Tablica),(M,N),El):- nth0(N,Tablica,H), nth0(M,H,X), El==X.

%Wpisywanie znaku Z na miejsce w tablicy dane wspolrzednymi (M,N):
wpisz((A,B,Tablica),(N,M),Znak,(A,B,Tabp)):- takeN(Tablica, M, (BT,[BH|BE])), takeN(BH, N, (EB, [EH|EE])), EH==wolne, append(EB, [Znak|EE], EN), append(BT,[EN|BE], Tabp).

% takeN(List, ElemC, (Beg, End)) - bierze ElemC elementow z List, zwraca je jako Beg a reszte jako End
takeN(L,     0, ([],L))   :-!.
takeN([H|T], X, ([H|B],E)):- Xp is X-1, takeN(T, Xp, (B,E)).

%Zamalowuje dane pola atomem w
zamalujPola([],    T,   T)   :-!.
zamalujPola([H|T], Tab, Tabp):- wpisz(Tab,H, w, Tabpp), zamalujPola(T, Tabpp, Tabp).

%Stwierdza czy dane pola sa wolne
wolnePola([],    _)  :-!.
wolnePola([H|T], Tab):- (\+ poleWTablicy(H, Tab); zajety(Tab, H, wolne)),!, wolnePola(T, Tab).

%Rozstrzyga czy w dane pole mozna wstawic staw (nowy, zatem nie powinien sasiadowac z innymi)
moznaWstawicWode((X,Y), Tablica):- Xg is X+1, Xd is X-1, Yg is Y+1, Yd is Y-1, wolnePola([(Xg,Y),(Xd,Y),(X,Yg),(X,Yd)], Tablica).

%Rozstrzyga czy dane pole nalezy do tablicy
poleWTablicy((X,Y), (M,N,_)):- X<M, X>=0, Y<N, Y>=0.

%Zwraca sasiada dowolnego pola z woda ze stawu danego lista wspolrzednych jego elementow 
sasiadPolaZWoda(_,       [],        _)      :- !, fail.
sasiadPolaZWoda((Xp,Yp), [(X,Y)|_], Tablica):- sasiad((X,Y),(Xp,Yp), Tablica).
sasiadPolaZWoda(S,       [_|T],     Tablica):- sasiadPolaZWoda(S, T, Tablica).

%Zwraca liste,w ktorej kazdy element z X wystepuje dokladnie raz
usunDuplikaty(X,Xp) :- sort(X,Xp), !.

%Laczy dwie posortowane listy (nie usuwa powtorzen)
merge([],    A,     A)     :- !.
merge(A,     [],    A)     :- !.
merge([A|D], [B|E], [C|F]) :-	(A@=<B ->  C=A, merge(D, [B|E], F); C=B, merge([A|D], E, F)	).

%Zwracaja kolejno: sasiada pola (X,Y) oraz liste jego sasiadow (w tablicy Tab)
sasiad((X, Y), (Xp,Yp), T):-(Xp=X, Yp is Y-1; Xp=X, Yp is Y+1; Yp=Y, Xp is X-1; Yp=Y, Xp is X+1), poleWTablicy((Xp,Yp), T).

sasiedzi(A, Tab, S):- findall(X, sasiad(A,X,Tab), Sp),!, sort(Sp, S).

%Wstawia staw dany lista wspolrzednych jego elementow i iloscia pol, ktore powinien jeszcze posiadac (nawroty daja kolejne mozliwosci, niestety z powtorzeniami)
wstawStaw(X, [S], T,Tp):- sasiedzi(S, T, Sa), wstawStaw(X, [S], Sa, T, Tp).

wstawStaw(0, Staw, _,     T,   Tp)  :- zamalujPola(Staw, T, Tp).
wstawStaw(X, Staw, [H|T], Tab, Tabp):- X>0, moznaWstawicWode(H, Tab), \+ member(H, Staw), Xp is X-1, sasiedzi(H, Tab, Sa), merge(Sa, T, Tpp), usunDuplikaty(Tpp,Tp), wstawStaw(Xp, [H|Staw], Tp, Tab, Tabp).
wstawStaw(X, Staw, [_|T], Tab, Tabp):- X>0, wstawStaw(X, Staw, T, Tab, Tabp).

%Przyjmuje liste stawow (danych jako wspolrzedne zrodla i ilosc pol) i wstawia je do tablicy T (generuje jedno przykladowe wstawienie, niestety wstawWstaw daje powtorzenia, stad findall aby sie ich pozbyc)
mapUzupelnijStawy([],            T,T) :-!.
mapUzupelnijStawy([(X,Y,Z)|Wsps],T,Tp):- moznaWstawicWode((X,Y), T), Zp is Z-1,  
  findall(Ti, wstawStaw(Zp,[(X,Y)],T, Ti), Tip), usunDuplikaty(Tip, Tis), member(Ti,Tis)
  %wstawStaw(Zp, [(X,Y)],T)
  , \+ \+ spojne((Wsps,Ti))
  , mapUzupelnijStawy(Wsps, Ti, Tp).

%Generuje kolejne mozliwosci rozwiazania problemu okreslonego danymi
generujMozliwosci(ParsedData, (ListaWspolrzednych, Tp)):- ParsedData = (ListaWspolrzednych, Tablica), mapUzupelnijStawy(ListaWspolrzednych, Tablica, Tp).

%Rozstrzyga, czy w tablicy nie ma wolnych pol
nieMaWolnego([]).
nieMaWolnego([H|T]) :- nieMaWolnegoRzad(H), nieMaWolnego(T).

%Rozstrzyga,czy w rzedzie nie ma wolnych pol
nieMaWolnegoRzad([]).
nieMaWolnegoRzad([H|T]):- H \= wolne, nieMaWolnegoRzad(T).

%Przyjmuje liste wierzcholkow do odwiedzenia i odwiedza ich oraz ich niezajetych sasiadow kolorujac wszystkich na czarno
dfsKolorujNaCzarno(Tw, [],    Tw):-!.
dfsKolorujNaCzarno(T,  [H|R], Tw):- 
  \+ zajety(T,H,c), wpisz(T,H,c,Tp), !, sasiedzi(H, T, Sa), append(Sa,R, Rp), 
    dfsKolorujNaCzarno(Tp, Rp,Tw).
dfsKolorujNaCzarno(T,  [_|R], Tw):- !, dfsKolorujNaCzarno(T,R,Tw).

%Znajduje pierwsze wolne pole w tablicy T zaczynajac od pola Wspa
znajdzWspWolnego(T, Wspa, Wsp) :- 
  poleWTablicy(Wspa, T), zajety(T, Wspa, wolne), !, Wsp=Wspa 
    ; Wspa=(M,N), T=(MG, NG, _), Mg is M+1, 
      (Mg < MG,!, znajdzWspWolnego(T,(Mg,N), Wsp); Ng is N+1, !, Ng < NG, znajdzWspWolnego(T, (0, Ng), Wsp)).

%Rozstrzyga, czy w rzedzie nie ma czarnych kwadratow
nieMaCzarnychKwadratowRzad([_],     [_])    .
nieMaCzarnychKwadratowRzad([c,c|_], [c,c|_]):-!, fail.
nieMaCzarnychKwadratowRzad([_|T1],  [_|T2]) :- nieMaCzarnychKwadratowRzad(T1,T2).

%Rozstrzyga, czy w tablicy nie ma czarnych kwadratow
nieMaCzarnychKwadratow([_]).
nieMaCzarnychKwadratow([X,Y|T]):- nieMaCzarnychKwadratowRzad(X,Y), nieMaCzarnychKwadratow([Y|T]).

%Rozstrzyga, czy danym rozwiazaniu groble sa spojne
spojne(Data) :- Data=(_, T), znajdzWspWolnego(T, (0,0), Wsp), dfsKolorujNaCzarno(T, [Wsp],Tp), !, (_,_,Tab) = Tp, nieMaWolnego(Tab).

%Rozstrzyga, czy dane rozwiazanie jest poprawne
sprawdzRozwiazanie(Data) :- Data=(_, T), spojne(Data), !, (_,_,Tab) = T, nieMaCzarnychKwadratow(Tab).

%Znajduje przykladowe rozwiazanie danych
znajdzWynik(ParsedData,W):- generujMozliwosci(ParsedData,M), sprawdzRozwiazanie(M), W=M.

%Zmienjsza pierwsze 2 wspolrzedne w danej liscie krotek o 1
mapMinusJedenWsp([],          [])            :-!.
mapMinusJedenWsp([(A,B,C)|T], [(Bp,Ap,C)|Tp]):- Ap is A-1, Bp is B-1, mapMinusJedenWsp(T,Tp).

%Parsuje dane
sparsujDane((M,N,Wsp), ParsedData):- gen(T,N,M), mapMinusJedenWsp(Wsp, Wspp), !,ParsedData = (Wspp, (N,M,T)).

%Wczytuje plik
wczytajPlik(F,Data):- seeing(Old), see(F), read(M), read(N), read(T), seen, see(Old), Data=(M,N,T),!.

wczytajNazwePliku(F):-write('Podaj nazwe pliku w formacie: \'<nazwa>\'. : '), read(F).

%Wypisuje elementy z rzedu sprawdzajac, czy nie powinna sie tam znalezc liczba (zgodnie z danymi)
wypiszRzad([],    _, _, _).
wypiszRzad([H|T], M, N, LW):- 
  (H==wolne,!, write('*'); H==w,!, (member((M,N,S),LW),!, write(S); write('0'))), Mp is M+1, wypiszRzad(T, Mp, N, LW).

%Wypisuje kolejne rzedy tablicy wyniku, pamietajac w ktorym rzedzie aktualnie sie znajduje
wypiszWynikAux(_,  _, _, [])   :-!.
wypiszWynikAux(LW, M, N, [H|T]):- wypiszRzad(H, M, N, LW), write('\n'), Np is N+1, wypiszWynikAux(LW, M,Np,T).

%Wypisuje caly wynik
wypiszWynik((LW,Wynik)):- Wynik=(_,_,Tab), wypiszWynikAux(LW, 0,0,Tab).

%%Glowny predykat programu. Ogolnie rozwiazanie zadania ma sie sprowadzac do kolejno generowania mozliwosci oraz sprawdzania czy spelniaja one kryteria, b y byc rozwiazaniem. Niestety nie udalo mi sie napisac generatora, ktory nie powtarzalby niektorych mozliwosci, wiec spamietuje rozwiazania i wypisuje jedynie te niepowtarzajace sie.
main:- wczytajNazwePliku(FileName), solve(FileName).

solve(FileName):- wczytajPlik(FileName,Data), sparsujDane(Data, ParsedData), findall(W, znajdzWynik(ParsedData,W), Ws), usunDuplikaty(Ws, Wsp), member(W, Wsp), wypiszWynik(W).

testWstawStaw(M,S):- gen(T, M,M), findall(P, wstawStaw(S, [(0,0)], (M,M,T), P),Ps),usunDuplikaty(Ps,Psp), member(P,Psp), print(P).

mapLadnieFormatuj([],[]):-!.
mapLadnieFormatuj([H|T],[Hp|Tp]):- ladnieFormatuj(H,Hp), mapLadnieFormatuj(T,Tp).

formatujRzad([],    _, _, _,  []).
formatujRzad([H|T], M, N, LW, [Hp|Tp]):- (H==wolne,!, Hp='*'; H==w,!, (member((M,N,S),LW),!, Hp=S; Hp=' ')), Mp is M+1, formatujRzad(T, Mp, N, LW,Tp).

ladnieFormatujAux(_,  _, _, [],[]):-!.
ladnieFormatujAux(LW, M, N, [H|T],[Hp|Tp]):- formatujRzad(H, M, N, LW, Hp), Np is N+1, ladnieFormatujAux(LW, M,Np,T,Tp).

ladnieFormatuj((LW,Wynik), Wp):- Wynik=(_,_,Tab), ladnieFormatujAux(LW, 0,0,Tab, Wp).

%Zgodnie ze specyfikacja zadania: wczytuje dane z pliku o podanej nazwie i zwraca liste wszystkich rozwiazan
%Aczkolwiek sugerowane jest raczej korzystanie z predykatu main lub solve, poniewaz wypisuja one wyniki ladniej sformatowane.
stawy(FileName, Solution):- wczytajPlik(FileName,Data), sparsujDane(Data, ParsedData), findall(W, znajdzWynik(ParsedData,W),Ws), mapLadnieFormatuj(Ws,Wsp), usunDuplikaty(Wsp, Solutions),!, member(Solution, Solutions).




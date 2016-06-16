%% Opis algorytmu przy predykacie main/0 (okolice linijki 140)
% Przypisuje literze cyfre (prawdopodobnie istnieje wbudowany predykat wykonujacy takie zadanie, ale ma rozne nazwy pod GNU i SWI Prologiem)
maplc(Lit,Cyf):-
     Lit='A',Cyf=0
  ;  Lit='B',Cyf=1
  ;  Lit='C',Cyf=2
  ;  Lit='D',Cyf=3
  ;  Lit='E',Cyf=4
  ;  Lit='F',Cyf=5
  ;  Lit='G',Cyf=6
  ;  Lit='H',Cyf=7
  ;  Lit='I',Cyf=8
  ;  Lit='J',Cyf=9
  ;  Lit='K',Cyf=10
  ;  Lit='L',Cyf=11
  ;  Lit='M',Cyf=12
  ;  Lit='N',Cyf=13
  ;  Lit='O',Cyf=14
  ;  Lit='P',Cyf=15
  ;  Lit='Q',Cyf=16
  ;  Lit='R',Cyf=17
  ;  Lit='S',Cyf=18
  ;  Lit='T',Cyf=19
  ;  Lit='U',Cyf=20
  ;  Lit='V',Cyf=21
  ;  Lit='W',Cyf=22
  ;  Lit='X',Cyf=23
  ;  Lit='Y',Cyf=24
  ;  Lit='Z',Cyf=25.

% Plansze reprezentuje przez tablice zmiennych, wstawiajac element do niej ukonkretniam dana zmienna, mechanizm prologowy dba aby te zmienne byly swieze
stworz_plansze(N,M,Tab):-
    M==0, !,
    Tab=[]
  ; Tab=[H|T],
    stworz_rzad(N,H), 
    Mp is M-1, 
    stworz_plansze(N,Mp,T).

stworz_rzad(N,Tab):-
    N==0, !, 
    Tab=[]
  ; Tab=[_|T], 
    Np is N-1, 
    stworz_rzad(Np, T).

% Wpisuje element (ukonkretnia zmienna tam sie znajdujaca)
% fail jesli jest zajety 
wpisz_element(N,M,Tab,El):- 
  nth0(N, Tab, H), 
  nth0(M,H,X), 
  %  var(X), 
  X=El.

% Sprawdza czy dane pole jest puste (tylko zmienna moze sie zunifikowac z inna zmienna)
% podwojne zaprzeczenie aby uniknac tworzenia nowych zmiennych
wolne_pole(N,M,Tab):- 
  \+ \+ wpisz_element(N,M,Tab,_).

% Kolejno generuje rzedy, upewniajac sie aby kazdy kolejny rzad spelnial warunki zadania
% Mozna to przyspieszyc dodajac dodatkowa tablice zliczajaca wystapienia kazdej litery i/lub zmienna mowiaca czy w jakim rzedzie byly identyczne litery
uzupelnij_wolne_pola(Tab, Zakres):-
    Tab==[], !
  ; Tab=[H|T], 
    uzupelnij_wolne_pola_w_rzedzie(H, Zakres), 
    tyle_samo_liter_lub_wszystkie_identyczne_w_rzedzie(H, Zakres),
    uzupelnij_wolne_pola(T,Zakres).

% Generuje kolejne elementy w tablicy, pilnujac aby nie wychodzily poza zakres dostepnych liter (Z - zakres liter)
uzupelnij_wolne_pola_w_rzedzie(Tab,Z):-
    Tab==[], !
  ; Tab=[H|T], 
    maplc(H,C),
    C=<Z,
    uzupelnij_wolne_pola_w_rzedzie(T,Z).

% Sprawdza, czy rzad R spelnia warunki zadania, Z - zakres danych liter
tyle_samo_liter_lub_wszystkie_identyczne_w_rzedzie(R,Z):-
    R=[H|T],
    wszystkie_litery_identyczne(H, T),!
  ; listazer(Z,L),
    tyle_samo_liter(R,L).

% Tworzy liste zer dlugosci Z
listazer(Z,L):-
  length(L,Z),
  wszystkie_litery_identyczne(0,L).

%Sprawdza czy wszystkie elementy w liscie sa identyczne z L
wszystkie_litery_identyczne(L, T):-
    T==[],!
  ; T=[L|T1],
    wszystkie_litery_identyczne(L, T1).

%Zwieksza Cty element listy L, dajac liste W 
zwieksz_nty_element(C,L,W):-
    C==0,!,
    L=[H|T],
    Hp is H+1,
    W=[Hp|T]
  ; Cp is C-1,
    L=[H|T],
    W=[H|Tp],
    zwieksz_nty_element(Cp,T,Tp).
    
%Upewnia sie ze wystapien kazdej litery w tablicy R jest tyle samo (kazdej, ktora wystepuje w danej tablicy oczywiscie) uzywajac pomocniczej tablicy L, w ktorej zapisuje ile razy kazda zmienna wystapila
tyle_samo_liter(R,L):-
    R==[],!, 
    member(H, L),
    H\=0,!,
    wszystkie_litery_identyczne_lub_zerowe(H,L)
  ; R=[H|T],
    maplc(H,C),
    zwieksz_nty_element(C,L,Lp),
    tyle_samo_liter(T,Lp).

%Upewnia sie ze kazdy element w tablicy T ma wartosc L lub 0
wszystkie_litery_identyczne_lub_zerowe(L, T):-
    T==[],!
  ; T=[X|T1],
  ( X==L,!
  ; X==0
  ) ,
    wszystkie_litery_identyczne_lub_zerowe(L, T1).

%Upewnia sie, ze wszystkie elementy przynajmniej jednym rzedzie danej tablicy sa takie same
wszystkie_litery_takie_same_w_jednym_rzedzie(T):-
    T==[],!,
    fail
  ; T=[H|Tp],
  ( H=[Hp|Tpp],
    wszystkie_litery_identyczne(Hp,Tpp),!
  ; wszystkie_litery_takie_same_w_jednym_rzedzie(Tp)
  ) .

%Sprawdza warunki zadania dla kolumn, X=1 oznacza, ze warunek nt identycznych liter nie byl zaspokojony dla rzedow, wiec musza go zaspokoic kolumny
sprawdz_kolumny(Table,Z,X):-
  obroc(Table, TableO),
  map_tyle_samo_liter_lub_identyczne(TableO,Z),
  ( X==1,!,
    wszystkie_litery_takie_same_w_jednym_rzedzie(TableO)
  ; var(X)
  ) .

%Obraca tablice o 90 stopni
obroc(T,TO):-
  length(T,M),
  T=[H|_],
  length(H,N),
  stworz_plansze(M,N, TO),
  przepisz_obrocone_wartosci(T,TO, 0,0, M,N).

% Przepisuje wszystkie wartosci z tablicy T do T0 w nastepujacy sposob: z pola (X,Y) do (Y,X)
% M - aktualnie rozwazany rzad; N - aktualnie rozwazana kolumna; Mmx, Nmx - zakresy M,N; 
przepisz_obrocone_wartosci(T, TO, M, N, Mmx, Nmx):-
    M=Mmx,!
  ; N=Nmx,!,
    Mp is M+1,
    przepisz_obrocone_wartosci(T,TO, Mp, 0, Mmx, Nmx)
  ; Np is N+1,
    wpisz_element(M, N, T, W),
    wpisz_element(N,M, TO, W),
    przepisz_obrocone_wartosci(T,TO,M,Np, Mmx, Nmx).

% Upewnia sie, ze wszystkie rzedy tablicy spelniaja warunki zadania, Z - zakres liter
map_tyle_samo_liter_lub_identyczne(T,Z):-
    T==[],!
  ; T=[H|Tp],
    tyle_samo_liter_lub_wszystkie_identyczne_w_rzedzie(H,Z),
    map_tyle_samo_liter_lub_identyczne(Tp,Z).

% Tworzy z danych strukture na ktorej program moze pracowac
przeksztalc(M,N,Dane, T):-
  stworz_plansze(M,N,T),
  map_wpisz_wartosci(Dane, T).

% Wpisuje dane do tablicy T
map_wpisz_wartosci(D, T):-
    D==[],!
  ; D=[(X,Y,Z)|T1], 
    Xp is X-1,
    Yp is Y-1,
    wpisz_element(Yp,Xp,T,Z),
    map_wpisz_wartosci(T1, T).

% Wypisuje dana tablice
wypisz(Table):-
    Table == [],!
  ; Table = [H|T],!,
    wypisz_rzad(H), 
    write('\n'), 
    wypisz(T).

wypisz_rzad(R):-
    R == [],!
  ; R = [L|T],!,
    write(L), 
    wypisz_rzad(T).

% Glowny predykat programu. Ogolnie rozwiazanie zagadki sprowadza sie do wygenerowania mozliwosci, spelniajacej czesc warunkow (ta dla rzedow), a nastepnie sprawdzenia czy spelania ona warunki dla kolumn. Poniewaz generowanie odbywa sie bez powotrzen, to nie trzeba nic spamietywac.
main:- 
  write('Podaj nazwe pliku z wejsciem\nw formacie \'<nazwa>\'. : '), 
  read(Filename), 
  rozwiaz(Filename, Table),
  wypisz(Table).

abc(Nazwa_pliku, Rozwiazanie):- 
  rozwiaz(Nazwa_pliku, Rozwiazanie).

rozwiaz(Nazwa_pliku, Rozwiazanie):- 
  seeing(Old),
  see(Nazwa_pliku),
  read(M),
  read(N),
  read(L),
  read(Data),
  seen,
  see(Old),
  write('Done loading.\n'),
  przeksztalc(N,M,Data, Table),
  maplc(L,Zp),
  Z is Zp+1,
  uzupelnij_wolne_pola(Table,Z),
  ( wszystkie_litery_takie_same_w_jednym_rzedzie(Table)
  ; X=1
  ) ,
  sprawdz_kolumny(Table,Z, X),
  Rozwiazanie = Table.

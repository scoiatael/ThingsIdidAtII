% SWI-Prolog -> tylko, uzyta biblioteka readutil (do latwego wczytywania) nie jest dostepna pod GNU Prologiem
% Predykat main uruchamia interfejs interpreters.
/* 
	Uwagi wstepne:
	
	Kod zarowno leksera, jak i parsera to wziety z KNO ogolnodostepny (przynajmniej dla uczeszczajacych na Programowanie L) lekser i parser jezyka While autorstwa prof Wierzbickiego.
	
	Dostosowanie leksera do zadanego jezyka ograniczylo sie do wyrzucenia duzej ilosci slow kluczowych, dodania nowych symboli i rozroznienia identyfikatorow na zmienne i konstruktory.
	
	Kod parsera z kolei pozycza jedynie sposob parsowania wyrazen arytmetycznych i wzor jak usunac lewostronna rekursje (bylo to konieczne, poza wyrazeniami, podczas parsowania aplikacji).
	
	Z kolei styl calego programu zostal utrzymany w stylu nadanym przez kod prof Wierzbickiego.
	
	Wiem, ze niemozliwe jest napisanie leksera i parsera zadanego jezyka w zasadniczo inny sposob (o ile parsuje sie po prostu do abstrakcyjnego drzewa rozbioru zadanego przez gramatyke, jak w tym przypadku),
	wiec mam nadzieje ze ten brak inwencji nie zostanie zbyt surowo potraktowany.
	
	Interpreter zostal napisany calkowicie samodzielnie, jednak wykorzystuje wiele pomyslow zarowno z programowania M, jak i wymyslonych przeze mnie i moich kolegow podczas dyskusji jak rozwiazac ta pracownie (jak np uzycie asserta do spamietywania wartosci, zastepowanie funkcji lokalnych globalnymi, zrzucenie arytmetyki na Prologa poprzez uzycie =.. i is dla obliczania wartosci wyrazenia niezaleznie od uzytego operatora).
	
	Dokladniejszy opis kazdego modulu znajduje sie nad nim. Zycze milej lektury.
	Lukasz Czaplinski.
	
*/


:- style_check(-atom).
:- use_module(library(readutil)).
:- dynamic('$val'/2).
:- dynamic(cache_size/1).

/*
   LEXICAL ANALYSIS

   Lexical structure:
      - symbols:     {, }, ;, \, ->, (, ), <, >, <=, >=, =, /=, +, -, *
      - constants:   nonempty sequences of digits 0 .. 9
      - keywords:    where div mod
      - identifiers: sequences of small and capital letters and digits and _ and '
                     that start with a letter and are different from
                     keywords
   Scanning assumes the maximal munch rule. Tokens can (and sometimes
   should) be separated with an arbitrary number of white space
   characters (spaces, tabs, newline characters etc.)
*/ 


lexer(Tokens) -->
   white_space, comment,
   (  (  "{",       !, { Token = tokLBrack }
	  ;  "}",       !, { Token = tokRBrack }
	  ;  "/=",      !, { Token = tokNeqEq }
      ;  ";",       !, { Token = tokSColon }
      ;  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
      ;  "+",       !, { Token = tokPlus }
      ;	"->",      !, { Token = tokFunc }  
      ;  "*",       !, { Token = tokTimes }
      ;  "=",       !, { Token = tokEq }
      ;  "\\",       !, { Token = tokLamb }
	  ;  "-",       !, { Token = tokMinus }
      ;  "<=",      !, { Token = tokLeq }
      ;  "<",       !, { Token = tokLt }
      ;  ">=",      !, { Token = tokGeq }
      ;  ">",       !, { Token = tokGt }
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (div, tokDiv),
                                     (mod, tokMod),
                                     (where, tokWhere)]),
               !
            ;  code_type(L, lower), !, Token = tokVar(Id)
			;  Token = tokConstr(Id)
            }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].
   
comment -->
	"--", stringEndingWithNewLine, !.
comment -->
	[].
	
stringEndingWithNewLine -->
	"\n", !.
stringEndingWithNewLine -->
	[_], stringEndingWithNewLine.
   
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, csym); A == 39}, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes( Id,[L|As] ) }.
	  
/*
   SYNTAX ANALYSIS

   Context-free grammar:

    <program> 	? 	<puste>
	<program> 	? 	<klauzula> <program>
	<klauzule> 	? 	<klauzula>
	<klauzule> 	? 	{ <program> }
	<klauzula> 	? 	<glowa klauzuli> = <wyrazenie> <deklaracje lokalne>
	<deklaracje lokalne> 	? 	;
	<deklaracje lokalne> 	? 	where <klauzule>
	<glowa klauzuli> 	? 	<zmienna> <parametry>
	<parametry> 	? 	<puste>
	<parametry> 	? 	<parametr> <parametry>
	<parametr> 	? 	<zmienna>
	<parametr> 	? 	<konstruktor>
	<parametr> 	? 	( <wzorzec> )
	<wzorzec> 	? 	<zmienna>
	<wzorzec> 	? 	<konstruktor> <parametry>
	<wyrazenie> 	? 	\ <zmienna> -> <wyrazenie>
	<wyrazenie> 	? 	<wyrazenie proste>
	<wyrazenie proste> 	? 	<wyrazenie proste> <operator binarny> <wyrazenie proste>
	<wyrazenie proste> 	? 	<aplikacja>
	<aplikacja> 	? 	<aplikacja> <wyrazenie atomowe>
	<aplikacja> 	? 	<wyrazenie atomowe>
	<wyrazenie atomowe> 	? 	( <wyrazenie> )
	<wyrazenie atomowe> 	? 	<zmienna>
	<wyrazenie atomowe> 	? 	<konstruktor>
	<zmienna> 	? 	identyfikator zaczynajacy sie mala litera
	<konstruktor> 	? 	identyfikator zaczynajacy sie wielka litera
	<konstruktor> 	? 	literal calkowitoliczbowy
	<operator binarny> 	? 	< | > | <= | >= | = | /= | + | - | * | div | mod

	Skladnia powyzsza jest niejednoznaczna. Przyjmujemy, ze operatory binarne maja nastepujace priorytety i kierunki lacznosci:
	Priorytet 	Lacznosc 	Operatory
	1 	w lewo 	*, div, mod
	2 	w lewo 	+, -
	3 	nielaczny 	<, >, <=, >=, =, /=
	
   To get a complete parser it suffices to replace character terminals
   in the grammar above with lexical tokens, eliminate left recursion and
   add appropriate semantic actions generating abstract syntax trees.
*/

program([K|P]) -->
	klauzula(K), !, program(P).
program([]) --> 
	[].

klauzule(K) --> 
	[tokLBrack], !, program(K), [tokRBrack].
klauzule([K]) --> 
	klauzula(K).

klauzula(clause(H,E,LD)) --> 
	glowaK(H), !, [tokEq], wyrazenie(E), deklaracjeL(LD).

deklaracjeL(K) --> 
	[tokWhere], !, klauzule(K).
deklaracjeL([]) --> 
	[tokSColon].

glowaK(head(Z,P)) --> 
	zmienna(Z), parametry(P).

parametry([H|T]) --> 
	parametr(H),!, parametry(T).
parametry([]) --> 
	[].

parametr(Z) --> 
	zmienna(Z).
parametr(C) --> 
	konstruktor(C).
parametr(W) --> 
	[tokLParen], wzorzec(W), [tokRParen].

wzorzec(matr(C,Pr)) --> 
	konstruktor(C), parametry(P), { reverse(P,Pr) }.
wzorzec(matr(Z)) --> zmienna(Z).

wyrazenie(f_lamb(Z,W)) --> 
	[tokLamb], zmienna(Z), [tokFunc], wyrazenie(W).
wyrazenie(W) --> 
	bool_expr(W).

bool_expr(W) --> 
	(	arith_expr(Ar1), rel_op(Op), !, 
			arith_expr(Ar2),{ W =.. [Op,Ar1,Ar2]}
	;	arith_expr(W) 
	).

arith_expr(Expr) -->
   summand(Summand), arith_expr(Summand, Expr).

arith_expr(Acc, Expr) -->
   additive_op(Op), !, summand(Summand),
      { Acc1 =.. [Op, Acc, Summand] }, arith_expr(Acc1, Expr).
arith_expr(Acc, Acc) -->
   [].

summand(Expr) -->
   factor(Factor), summand(Factor, Expr).

summand(Acc, Expr) -->
   multiplicative_op(Op), !, factor(Factor),
      { Acc1 =.. [Op, Acc, Factor] }, summand(Acc1, Expr).
summand(Acc, Acc) -->
   [].

factor(Expr) -->
   aplikacja(Expr).
   
aplikacja(Appl) -->
	wyrazenie_atom(Wyr), !, aplikacja(Wyr,Appl).
aplikacja(Acc, Appl) --> 
	wyrazenie_atom(Wyr), !, { Acc1 =.. [appl, Acc,Wyr] },
		aplikacja(Acc1,Appl).
aplikacja(Acc,Acc) --> 
	[].
	
wyrazenie_atom(W) --> 
	(	[tokLParen], !, wyrazenie(W), [tokRParen]
	;	zmienna(W), !
	;	konstruktor(W)
	).

zmienna(var(V)) --> 
	[tokVar(V)].

konstruktor(constr(C)) --> 
	[tokConstr(C)], !.
konstruktor(constr(num(L)))  --> 
	[tokNumber(L)].

additive_op(+) -->
   [tokPlus], !.
additive_op(-) -->
   [tokMinus].

multiplicative_op(*) -->
   [tokTimes], !.
multiplicative_op(div) -->
   [tokDiv], !.
multiplicative_op(mod) -->
   [tokMod].

rel_op(=) -->
   [tokEq], !.
rel_op(\=) -->
   [tokNeqEq], !.
rel_op(<) -->
   [tokLt], !.
rel_op(=<) -->
   [tokLeq], !.
rel_op(>) -->
   [tokGt], !.
rel_op(>=) -->
   [tokGeq].

parse(CharCodeList, Absynt) :- 
	phrase(lexer(Tokens), CharCodeList),
	phrase(program(Absynt), Tokens).

/* 
	INTERPRETER
	
	1) Wczytywanie danych:
		i) wczytuje podany plik do stringa
		ii) parsuje go
		iii) program jest reprezentowany przez liste klauzul, wiec dodanie go do bazy danych polega na:
			a) wyrzuceniu ze starej bazy klauzul o tej samej nazwie, co jedna z wlasnie wczytanych
			b) dodaniu do pozostalych nowowczytanych klauzul
	2) Obliczanie wartosci wyrazenia:
		i) zaleznie od rodzaju wyrazenia:
			- aplikacja -> oblicza funkcje i wstawia do niej wyrazenie
			- wyrazenia polaczone operatorem binarnym -> oblicza je, nastepnie aplikuje operator
			- konstruktor -> nie oblicza
			- zmienna -> przeszukuje baze wiedzy w poszukiwaniu tak nazwanej funkcji i zastepuja nie zmienna, w przypadku niepowodzenia pozostawia bez zmian
		ii) wstawianie wyrazenia do funkcji zalezy od tego, pod co nalezy je podstawic w funkcji:
			- zmienna -> zastepuje wystapienia zmiennej w funkcji wyrazeniem
			- konstruktor -> oblicza wyrazenie i porownuje je z konstruktorem:
				- identyczne -> powodzenie
				- rozne -> niepowodzenie (prolog nawroci do ostatniego rozgalezienia, czyli podstawiania funkcji pod zmienna
			- wzorzec -> oblicza wyrazenie, porownuje jego korzen z korzeniem wzorca a nastepnie podstawia wyrazenia z wzorca pod odpowiednie zmienne w wyrazeniu
		iii) funkcje lokalne sa rozwiazane poprzed "podniesienie ich" do globalnych, ze zmiana nazwy (dodaniu nazwy funkcji macierzystej i & - znak ten nie moze sie pojawic w normalnych nazwach)
		iv) wzorce - rozwiazano dzieki mechanizmowi nawrotow prologa - w przypadku, gdy wartosc podstawiana nie zgadza sie z wzorcem, predykat zawodzi, a sam prolog nawraca do najblizszego mozliwego rozgalezienia (ktore dzieki dobremu zastotoswaniu odciec jest przeszukaniem bazy danych w poszukiwaniu definicji funkcji, a dzieki spamietywaniu policzone wartosci sa latwo odtwarzane)
		
		Uwagi
		i) funkcje sa dynamiczne, jednak mozliwe jest uzyskanie statycznych - wystarczy wykorzystac gotowy mechanizm zmiany nazw w funkcjach (wykorzystany w funkcjach lokalnych) do grupowania wczytywanych predykatow - wszystkie wczytane w tym samym pliku powinny dostac ten sam numer, wiekszy od dotychczasowych, i odwolywac sie tylko do funkcji z takim samym lub nizszym numerem
		ii) uzyto asserta i retractall do "naiwnego" spamietywania - zapamietywana jest wartosc policzonej wlasnie funkcji, o ile jest ona konkretna (wartosc jest konstruktorem, badanie czy wieksze wyrazenia sa ukonkretnione do konca/ zapamietywanie niekonkretnych nie daje zbyt duzego zysku)
			- dzieki temu np naiwne obliczanie ciagu fibonacciego jako:
				fibaux 0 = 1;
				fibaux 1 = 1;
				fibaux n = (fibaux (n-1) + fibaux(n-2));
				fib = map fibaux (enumFrom 0);
			jest szybsze od przykladowego z kno
			- wynika to nie z faktu, ze zapamietujemy po prostu kolejne wartosci ciagu fibonacciego (wymagaloby to wymuszenia na prologu ukonkretniania argumentow funkcji), lecz z przyspieszenia arytmetyki - zapamietujemy wartosci wyrazen i uzywamy ich wielokrotnie.
		iii) niemniej jednak wszystkie funkcje przykladowe z kno dzialaja, testy byly prowadzone na netbooku wiec mozliwe bylo tylko okreslenie relatywnej predkosci dzialania. Jak pisalem, dzieki spamietywaniu naiwne funkcje numeryczne sprawdzaja sie najlepiej.
*/

/* 1ii) - iii) przetwarzanie nowych danych do bazy (wczytywanie nastepuje w predykacie sterujacym - minimazliuje ilosc predykatow operujacych na zewnetrznych danych) */
process(CharCodeListT, Base, New_Base) :-
   append(CharCodeListT, "\n", CharCodeList), 
   parse(CharCodeList, Absynt), 
   usunFLok(Absynt, Wynik, '&Nil', []),
   update(Wynik, Base, New_Base).

usunFLok(In, Out, Str, GPar):- 
	(	In = [H1|T1], Out = [H2|T2], !, H1 = clause(head(var(N), Par), Wyr, DL), append(Par, GPar, ParT), 		appendA(Str,N,N1), appendA(&, N, StrN), appendA(StrN, Str, StrNN), 
			zmienNaz(Wyr, Wyr1, StrNN, DL, ParT), usunFLok(DL, DLN, StrNN, ParT), 
				eachZmienNaz(DLN, DLNN, StrNN, DL, Par), append(DLNN, TT,T2), 
				H2 = clause(head(var(N1), ParT), Wyr1), usunFLok(T1, TT, Str, GPar)
	;	In = [], !, Out = [] 
	).
			
appendA(Atom1, Atom2, Atom3):- 
	(	Atom1 = '&Nil', !, Atom3 = Atom2
	;	Atom2 = '&Nil', !, Atom3 = Atom1
	;	atom_codes(Atom1, C1), atom_codes(Atom2, C2), 
			append(C1,C2,C3), atom_codes(Atom3,C3)
	).
	
eachZmienNaz(In, Out, Str, DL, Par):-
	(	In = [], !, Out = []
	;	In = [H1|T1], !, Out = [H2|T2], H1 = clause(head(H,P),Wyr), zmienNaz(Wyr, WyrW, Str, DL, Par),
			H2 = clause(head(H,P), WyrW), eachZmienNaz(T1, T2, Str, DL, Par)
	).
	
zmienNaz(Wyr, WyrW, Str, DL, Par):-
	(	Wyr =.. [Op, Wyr1, Wyr2], !, zmienNaz(Wyr1, Wyr1W, Str, DL, Par), 
			zmienNaz(Wyr2, Wyr2W, Str, DL, Par), WyrW =.. [Op, Wyr1W, Wyr2W]
	;	Wyr = constr(_), !, WyrW = Wyr
	;	Wyr = var(N), !, 
		(	member(clause(head(Wyr, _), _, _), DL), !, appendA(Str, N, NN), reverse(Par, RPar),
				addParams( var(NN), RPar, WyrW)
		;	WyrW = Wyr
		)
	).
	
addParams(S, Par, WyrW):-
	(	Par = [], !, WyrW = S
	;	Par = [H|T], !, WyrW = appl(Temp, H), addParams(S, T, Temp)
	).
	
update( Absynt, Base, New_Base):-
	usun(Base, Absynt, Base_Temp), append(Base_Temp, Absynt, New_Base).


usun(Base, List, New_Base):- 
	(	List = [], !, New_Base = Base
	;	List = [clause(head(H, _), _)|T], !, usun_el(Base, H, Temp), usun(Temp, T, New_Base)
	).

usun_el(In, Name, Out):- 
	(	In = [], !, Out = []
	;	In = [clause(head(H, _), _)|T], !, usun_el(T, Name, Out)
	;	In = [H|I1], !, Out = [H|O1], usun_el(I1, Name, O1)
	).
   
/* 2ii) oblicza aplikacje funkcji poprzez wstawienie wyrazen w miejsce parametrow w ciele funkcji*/
wstaw(WyrAt, Funk, FunkWyn, Base):-
	Funk = clause(head(_, [H|Par]), Wyr), !, 
		wstaw(WyrAt, Wyr, FunkWynTemp, H, Base), 
		(	Par == [], !, ukonkretnij(FunkWynTemp, FunkWyn, Base) 
		;	FunkWyn = clause(head(_,Par), FunkWynTemp)
		).
	
wstaw(WyrAt, Wyr, WyrWyn, Var, Base):-
	(	Var = var(H),
		(	Wyr = f_lamb(X,WyrL),
			(	Var = X, !, WyrWyn = Wyr
			;	wstaw(WyrAt, WyrL, WyrLWyn, Var, Base), WyrWyn = f_lamb(X, WyrLWyn)
			)
		;	Wyr = [], !, WyrWyn = Wyr
		;	Wyr = [H|T], !, wstaw(WyrAt, H, HWyn, Var, Base), wstaw(WyrAt, T, TWyn, Var, Base),
				WyrWyn = [HWyn|TWyn]
		;	Wyr =.. [Op, Arg1, Arg2], !, wstaw(WyrAt, Arg1, Arg1Wyn, var(H), Base), 
				wstaw(WyrAt, Arg2, Arg2Wyn, var(H), Base), WyrWyn =.. [Op, Arg1Wyn, Arg2Wyn]
		;	Wyr = var(Zm), !,
			(	Zm == H, !, WyrWyn = WyrAt
			;	WyrWyn = Wyr
			)
		;	Wyr = constr(C), !, WyrWyn = constr(C)
		;	Wyr = clause(head(Name,Par), SubWyr), !,
			(	member(var(H),Par),!
			;	wstaw(WyrAt, SubWyr, SubWyrWyn, var(H), Base), 
					WyrWyn = clause(head(Name,Par), SubWyrWyn)
			)
		)
	;	Var = constr(H), !, ukonkretnij(WyrAt, WyrAt1, Base), 
			WyrAt1 == constr(H), WyrWyn = Wyr
	;	Var = matr(C,P), !, ukonkretnij(WyrAt, WyrAt1, Base), 
			WyrAt1 = matr(C, P1), wstaw(P1, Wyr, WyrWyn, P, Base)
	;	Var = matr(Z), !, wstaw(WyrAt, Wyr, WyrWyn, Z, Base)
	;	Var = [], WyrAt = [], !, WyrWyn = Wyr, !
	;	Var = [H2|T2], WyrAt = [H1|T1], !, wstaw(H1, Wyr, Wyr1, H2, Base), 
			wstaw(T1, Wyr1, WyrWyn, T2, Base)
	). 

/* 2iii) i iv) ukonkretnianie korzenia wyrazenia i wypisanie wyniku */
print_proc(ValT, Base):-
	ukonkretnij(ValT, Val, Base),
	(	Val = clause(head(_,T), Wyr), !,
		(	T \= [], !, atom_codes(Atom, "<function>"), print(Atom),nl
		;	print_proc(Wyr, Base)
		)
	;	Val = constr(num(V)), !, print(V), nl
	;	Val = constr(C), !, print(C), nl
	;	Val = matr(C, P), !, reverse(P,Pr), atom_codes(A,"("), print(A), print_list([C|Pr], Base), 
			atom_codes(B,")"), print(B),nl
	;	print(failed1ToCompute-->Val)
	).
	
print_list(Val, Base):-
	(	Val = [], !
	;	Val = [H1|Tail], !, ukonkretnij(H1, H, Base),
		(	H = clause(head(_,T), Wyr), !, 
			(	T \= [], !, atom_codes(Atom, "<function>"), print(Atom)
			;	print_list([Wyr], Base)
			)
		;	H = constr(num(V)), !, print(V)
		;	H = constr(C), !, print(C)
		;	H = matr(C, P), !, reverse(P,Pr), atom_codes(A,"("), print(A), print_list([C|Pr], Base), 
				atom_codes(B,")"), print(B)
		;	print(failed2ToCompute-->H)
		), atom_codes(Atom, " "), print(Atom), print_list(Tail, Base)
	).
	
ukonkretnij(Wyr, Val, Base):-
%	(	Wyr \= constr(_), '$val'(Wyr, Val), !
		(	Wyr = clause(head(_,[]), WyrF), !, ukonkretnij(WyrF, ValF, Base), 
				Val = ValF
		;	Wyr = matr(_,_), !, Val = Wyr
		;	Wyr = appl(F,WyrAt), !, ukonkretnij(F, ValF, Base), 
			(	ValF = constr(X), !, Val = matr(constr(X), [WyrAt])
			;	ValF = matr(C, T), !, Val = matr(C, [WyrAt|T])
			;	ValF = clause(head(_,P), WyrL),
			(	P == [], !, ukonkretnij(appl(WyrL, WyrAt), Val, Base)
				;	wstaw(WyrAt, ValF, Val, Base)
				)
			)
		;	Wyr = f_lamb(Z,WyrF), !, Val = clause(head(_, [Z]), WyrF)
		;	Wyr = var(X), !,
			(	member(clause(head(Wyr, Par), WyrF), Base), 
					(	Par = [], !, gtrace, ukonkretnij(WyrF, Val, Base) 
					;	Val = clause(head(Wyr, Par), WyrF)
					)
			;	\+ member(clause(head(Wyr, _), _, _), Base), Val = var(X)
			)
		;	Wyr =.. [Op, Wyr1, Wyr2], !,
			(	member(Op, [<, >, <=, >=, =, \=]), !, ukonkretnij(Wyr1, constr(num(Val1)), Base),
					ukonkretnij(Wyr2, constr(num(Val2)), Base), Funk =.. [Op, Val1, Val2],
					(	Funk,!, atom_codes(True, "True"), Val = constr(True)
					;	atom_codes(False, "False"), Val = constr(False)
					)
			;	member(Op, [+, -, *, mod, div]), ukonkretnij(Wyr1, constr(num(Val1)), Base), 
					ukonkretnij(Wyr2, constr(num(Val2)), Base), 
						Funk =.. [Op, Val1, Val2], V is Funk, Val = constr(num(V))
			)
		;	Val = Wyr
		)
		,
		(	jest_konkr(Val), Wyr \= constr(_), !, assert('$val'(Wyr, Val)), retract(cache_size(X)), 
					XN is X+1, assert(cache_size(XN))
		;	true
		)
	.
	
jest_konkr(Val):-
	(	Val = [], !
	;	Val = [H|T], !, jest_konkr(H), jest_konkr(T)
	;	Val = constr(_),!
	).
	
/* Predykat sterujacy praca interpretera.
Obsluguje on nastepujace polecenia:
:l <nazwa_pliku> - dodaje do bazy danych klauzule z pliku, nadpisuje wszystkie majace takie same nazwy jak dowolna z nowowczytanych
:e <wyrazenie> - oblicza wartosc wyrazenia
:q - zamyka interpreter 
:p - wypisuje aktualna baze danych
:c - czysci baze danych 
:z <nazwa_pliku> - wypisuje wynik dzialania leksera na podanym pliku
*/
process_inpt(Base, New_Base):- 
	atom_codes(Atom, ">"), print(Atom), get_code(C1), 
	(	C1 = -1, !, fail
	;	get_code(C2), InptStr =[C1,C2], get_code(_),
		(	InptStr == ":l", !, read_line_to_codes(user_input, FileName), atom_codes(File, FileName), 
				read_file_to_codes(File, Codes, []), 
				(	process(Codes, Base, New_Base), !
				;	print(bladWczytywanieSieNiePowiodlo), nl, New_Base = Base
				)
		;	InptStr == ":e", !, read_line_to_codes(user_input, Expression),
				(	phrase(lexer(Tokens), Expression), assert(cache_size(0)),
						phrase(wyrazenie(Val), Tokens), !,
						(	print_proc(Val, Base), !
						;	print(bladObliczania),nl
						) 
				;	print(bladParsowania),nl
				),	cache_size(X),
/* Po obliczeniu wyrazenia wypisuje ilosc spamietanych wartosci (czyli predykatow dodanych 
do bazy danych w trakcie liczenia. Predykaty te sa usuwane po wypisaniu wartosc wyrazenia */				
%				print('cache size is'(X)), nl,
						retractall('$val'), retractall(cache_size), New_Base = Base
		;	InptStr == ":q", !, fail
		; 	InptStr == ":p", !, print(Base), nl,  New_Base = Base
		; 	InptStr == ":c", !, New_Base = []
		;	InptStr == ":z", !, read_line_to_codes(user_input, FileName), atom_codes(File, FileName), 
				read_file_to_codes(File, CodesT, []), append(CodesT, "\n", Codes),
					phrase(lexer(T), Codes), print(Codes), nl,
						print(T), nl, New_Base = Base
		)
	).
	
/* Predykat zapetlajacy (pozwalajacy na wykonanie wiecej niz jednej instrukcji) dzialanie interpretera */
loop(BB) :- 
	process_inpt(BB, NB), loop(NB).

/* Glowny predykat uruchamiajacy program */
main :- 
	loop([]).
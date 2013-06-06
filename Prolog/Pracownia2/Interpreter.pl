% SWI-Prolog

:- style_check(-atom).
:- use_module(library(readutil)).

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
   white_space,
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
      { atom_codes(Id, [L|As]) }.
	  
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

klauzula(clause(H,E,(LD,LD))) --> 
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
rel_op(/=) -->
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
		iv) 
*/

/* 1ii) - iii) przetwarzanie nowych danych do bazy (wczytywanie nastepuje w predykacie sterujacym - minimazliuje ilosc predykatow operujacych na zewnetrznych danych) */
process(CharCodeList, Base, New_Base) :-
   parse(CharCodeList, Absynt),
   dodajEnvf(Absynt, (_, Absynt), Wynik),
   update(([], Wynik), Base, New_Base).

dodajEnvf([clause(head(Z,P),W,B)|T], Base, [clause(head(Z,P), W, New_B)|New_T]):-
	update(B, Base, New_B), dodajEnvf(T,Base,New_T).   
dodajEnvf([], _, []):-!.

update((D, Absynt), (_, Base), (D, New_Base)):-
	usun(Base, Absynt, Base_Temp), append(Base_Temp, Absynt, New_Base).

	
/* DBG */
printS([]):-!.
printS([clause(head(Z,P), _, _)|T]):- !,print(head(Z,P)), nl, printS(T).
printS(clause(head(Z,P), _, _)):- !,print(head(Z,P)), nl.
printS(Val):- Val =.. [o1,o2,o3], !,printS(o1), printS(o2), printS(o3).
printS(V) :- print(V),!,nl.
/* --- */	

usun(Base, [], Base):- !.
usun(Base, [clause(head(H,_),_,_)|T], New_Base):- usun_el(Base, H, Temp), usun(Temp, T, New_Base).
usun_el([], _, []):- !.
usun_el([clause(head(H,_),_,_)|T], H, P):- !, usun_el(T, H, P).
usun_el([X|T], H, [X|P]):- usun_el(T, H, P).
   
/* 2ii) oblicza aplikacje funkcji poprzez wstawienie wyrazen w miejsce parametrow w ciele funkcji*/
wstaw(WyrAt, Funk, FunkWyn, Base, Final_Base):-
	Funk = clause(head(_, [H|Par]), Wyr, BaseL), !, 
		update(BaseL, Base, New_Base), wstaw(WyrAt, Wyr, FunkWynTemp, H, New_Base, Final_1_Base), 
		(	Par == [], !, ukonkretnij(FunkWynTemp, FunkWyn, Final_1_Base, Final_Base) 
		;	FunkWyn = clause(head(_,Par), FunkWynTemp, Final_1_Base), Final_Base = Final_1_Base
		).
	
wstaw(WyrAt, Wyr, WyrWyn, var(H), Base, Final_Base):-
	(	Wyr = f_lamb(_,_), WyrWyn = Wyr	, Final_Base = Base
	;	Wyr =.. [Op, Arg1, Arg2], !, wstaw(WyrAt, Arg1, Arg1Wyn, var(H), Base, Final_1_Base), 
			wstaw(WyrAt, Arg2, Arg2Wyn, var(H), Final_1_Base, Final_Base), WyrWyn =.. [Op, Arg1Wyn, Arg2Wyn]
	;	Wyr = var(Zm), !,
		(	Zm == H, !, WyrWyn = WyrAt, Final_Base = Base
		;	Base = (D,_), member(clause(head(Wyr,_),_,_), D), !, 
				ukonkretnij(var(Zm), VarWyn, Base, Final_1_Base), 
				(	VarWyn == var(Zm), !, WyrWyn = var(Zm), Final_Base = Final_1_Base
				;	wstaw(WyrAt, VarWyn, WyrWyn, var(H), Final_1_Base, Final_Base)
				)
		;	WyrWyn = Wyr, Final_Base = Base
		)
	;	Wyr = constr(C), !, WyrWyn = constr(C), Final_Base = Base
	;	Wyr = clause(head(Name,Par), SubWyr, BaseL), !,
		(	member(var(H),Par),!
		;	update(BaseL, Base, New_BL), wstaw(WyrAt, SubWyr, SubWyrWyn, var(H), New_BL, Final_Base), 
				WyrWyn = clause(head(Name,Par), SubWyrWyn, Final_Base)
		)
	).

wstaw(WyrAt, Wyr, WyrWyn, constr(H), Base, Final_Base):-
	ukonkretnij(WyrAt, WyrAt1, Base, Final_Base), WyrAt1 == constr(H), WyrWyn = Wyr. 
wstaw(WyrAt, Wyr, WyrWyn, matr(C,P), Base, Final_Base):-
	!, ukonkretnij(WyrAt, WyrAt1, Base, Final_1_Base), WyrAt1 = matr(C, P1), 
		wstaw(P1, Wyr, WyrWyn, P, Final_1_Base, Final_Base).
wstaw(WyrAt, Wyr, WyrWyn, matr(Z), Base, Final_Base):-
	wstaw(WyrAt, Wyr, WyrWyn, Z, Base, Final_Base).
wstaw([], Wyr, Wyr, [], Base, Final_Base):- !, Final_Base = Base.
wstaw([H1|T1], Wyr, WyrWyn, [H2|T2], Base, Final_Base):-
	wstaw(H1, Wyr, Wyr1, H2, Base, Final_1_Base), wstaw(T1, Wyr1, WyrWyn, T2, Final_1_Base, Final_Base). 

/* 2iii) i iv) ukonkretnianie korzenia wyrazenia i wypisanie wyniku */
print_proc(ValT, Base):-
	ukonkretnij(ValT, Val, Base, Final_Base),
	(	Val = clause(head(_,T), Wyr, BaseL), !,
		(	T \= [], !, atom_codes(Atom, "<function>"), print(Atom),nl
		;	update(BaseL, Final_Base, New_BL), print_proc(Wyr, New_BL)
		)
	;	Val = constr(num(V)), !, print(V), nl
	;	Val = constr(C), !, print(C), nl
	;	Val = matr(C, P), !, reverse(P,Pr), atom_codes(A,"("), print(A), print_t([C|Pr], Final_Base), 
			atom_codes(B,")"), print(B),nl
	;	print_proc(Val, Base)
	).
	
print_t(Val, Base):-
	(	Val = [], !
	;	Val = [H1|Tail], !, ukonkretnij(H1, H, Base, Final_Base),
		(	H = clause(head(_,T), Wyr, BaseL), !, 
			(	T \= [], !, atom_codes(Atom, "<function>"), print(Atom)
			;	update(BaseL, Final_Base, New_BL), print_t([Wyr], New_BL)
			)
		;	H = constr(num(V)), !, print(V)
		;	H = constr(C), !, print(C)
		;	H = matr(C, P), reverse(P,Pr), atom_codes(A,"("), print(A), print_t([C|Pr], Final_Base), 
				atom_codes(B,")"), print(B)
		;	print(H)
		), atom_codes(Atom, " "), print(Atom), print_t(Tail, Final_Base)
	).
	
ukonkretnij(Wyr,Val, Base, Final_Base):- 
/*	print(u_),printS(Wyr),nl,*/
	(	Wyr = clause(head(_,[]), WyrF, B), !, update(B,Base,New_B), 
			ukonkretnij(WyrF, ValF, New_B, Final_Base), Val = ValF
	;	Wyr = appl(F,WyrAt), !, ukonkretnij(F, ValF, Base, Temp_Base), 
		(	ValF = constr(X), !, Val = matr(constr(X), [WyrAt]), Final_Base = Temp_Base
		;	ValF = matr(C, T), !, Val = matr(C, [WyrAt|T]), Final_Base = Temp_Base
		;	ValF = clause(head(_,P), WyrL, BaseL), update(BaseL, Temp_Base, New_B),
			(	P == [], !, ukonkretnij(appl(WyrL, WyrAt), Val, New_B, Final_Base)
			;	wstaw(WyrAt, ValF, Val, New_B, Final_Base)
			)
		)
	;	Wyr = f_lamb(Z,WyrF), !, Val = clause(head(_, [Z]), WyrF, Base), Final_Base = Base
	;	Wyr = var(X), !, Base = (_,B),
		(	member(clause(head(Wyr, Par), WyrF, BaseL), B),
				update(BaseL,Base,Final_1_Base), 
				(	Par = [], !, ukonkretnij(WyrF, Val, Final_1_Base, Final_Base) 
				;	Final_Base = Final_1_Base, Val = clause(head(Wyr, Par), WyrF, Final_Base)
				)
		;	\+ member(clause(head(Wyr, _), _, _), B), Val = var(X), Final_Base = Base
		)
	;	Wyr =.. [Op, Wyr1, Wyr2], !,
		(	member(Op, [<, >, <=, >=, =, /=]), !, ukonkretnij(Wyr1, constr(num(Val1)), Base, Final_1_Base),
				ukonkretnij(Wyr2, constr(num(Val2)), Final_1_Base, Final_Base), Funk =.. [Op, Val1, Val2],
				(	Funk,!, atom_codes(True, "True"), Val = constr(True)
				;	atom_codes(False, "False"), Val = constr(False)
				)
		;	member(Op, [+, -, *]), ukonkretnij(Wyr1, constr(num(Val1)), Base, Final_1_Base), 
				ukonkretnij(Wyr2, constr(num(Val2)), Final_1_Base, Final_Base), 
					Funk =.. [Op, Val1, Val2], V is Funk, Val = constr(num(V))
		)
	;	Val = Wyr, Final_Base = Base	
	)
/*	,print(u_), printS(Wyr),print(w_),printS(Val), nl,get_code(_) */
	.
	
/* Predykat sterujacy praca interpretera */
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
				(	phrase(lexer(Tokens), Expression), phrase(wyrazenie(Val), Tokens), !,
					(	print_proc(Val, Base), !
					;	print(bladObliczania),nl
					) 
				;	print(bladParsowania),nl
				),	New_Base = Base
		;	InptStr == ":q", !, fail
		; 	InptStr == ":p", !, print(Base), nl,  New_Base = Base
		; 	InptStr == ":c", !, New_Base = []
		)
	).
	
/* Predykat zapetlajacy (pozwalajacy na wykonanie wiecej niz jednej instrukcji) dzialanie interpretera */
loop(BB) :- 
	process_inpt(BB, NB), loop(NB).

/* Glowny predykat uruchamiajacy program */
main :- 
	loop(([],[])).
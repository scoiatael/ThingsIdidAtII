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
      ;  "-",       !, { Token = tokMinus }
      ;  "*",       !, { Token = tokTimes }
      ;  "=",       !, { Token = tokEq }
      ;  "\\",       !, { Token = tokLamb }
	  ;  "->",      !, { Token = tokFunc }
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
parametr(matr(W)) --> 
	[tokLParen], wzorzec(W), [tokRParen].

wzorzec([C,P]) --> 
	konstruktor(C), parametry(P).

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
		i) zastepuje zmienne w wyrazeniu odpowiednim funkcjami (klauzulami) z bazy danych
		ii) oblicza aplikacje funkcji poprzez wstawienie wyrazen w miejsce parametrow w ciele funkcji
		ii*) w razie natrafienia na konstruktor ukonkretnia (oblicza) korzen wyrazenia
		iii) ukonkretnia korzen glownego wyrazenia
		iv) wypisuje go
*/

/* 1ii) - iii) przetwarzanie nowych danych do bazy (wczytywanie nastepuje w predykacie sterujacym - minimazliuje ilosc predykatow operujacych na zewnetrznych danych) */
process(CharCodeList, Base, New_Base) :-
   parse(CharCodeList, Absynt),
   print(Absynt),
   update(Absynt, Base, New_Base).
   
update(Absynt, Base, New_Base):-
	usun(Base,Absynt, Base_Temp), append(Base_Temp, Absynt, New_Base).

usun(Base, [], Base).
usun(Base, [clause(head(H,_),_,_)|T], New_Base):- usun_el(Base, H, Temp), usun(Temp, T, New_Base).
usun_el([], _, []).
usun_el([clause(head(H,_),_,_)|T], H, P):- !, usun_el(T, H, P).
usun_el([X|T], H, [X|P]):- usun_el(T, H, P).
   
/* 2ii) oblicza aplikacje funkcji poprzez wstawienie wyrazen w miejsce parametrow w ciele funkcji*/
wstaw(WyrAt, Funk, FunkWyn):-
	Funk = clause(head(_, [H|Par]), Wyr, []), !, wstaw(WyrAt, Wyr, FunkWynTemp, H), 
		FunkWyn = clause(head(_,Par), FunkWynTemp, []).
	
wstaw(WyrAt, Wyr, WyrWyn, var(H)):-
	(	Wyr =.. [Op, Arg1, Arg2], !, wstaw(WyrAt, Arg1, Arg1Wyn, var(H)), 
			wstaw(WyrAt, Arg2, Arg2Wyn, var(H)), WyrWyn =.. [Op, Arg1Wyn, Arg2Wyn]
	;	Wyr = var(Zm), !,
		(	Zm == H, !, WyrWyn = WyrAt
		;	WyrWyn = var(Zm)
		)
	;	Wyr = constr(C), WyrWyn = constr(C)
	).
wstaw(WyrAt, Wyr, WyrWyn, constr(H)):-
	ukonkretnij(WyrAt, WyrAt1), WyrAt1 == constr(H), WyrWyn = Wyr. 
wstaw(WyrAt, Wyr, WyrWyn, matr(C,P)):-
	ukonkretnij(WyrAt, WyrAt1), WyrAt1 == matr(C, P1), wstaw(P1, Wyr, WyrWyn, P).
wstaw([], Wyr, Wyr, []):- !.
wstaw([H1|T1], Wyr, WyrWyn, [H2|T2]):-
	wstaw(H1, Wyr, Wyr1, H2), wstaw(T1, Wyr1, WyrWyn, T2). 
	
/* 2i) - zamienia nazwy funkcji na ich pelna tresc*/
eval(Wyr, Base, Wyn) :-
	(	Wyr = appl(F, WyrAt), !, eval(F, Base, FWyn),
			eval(WyrAt, Base, WyrAtWyn), Wyn = appl(FWyn, WyrAtWyn)
	;	Wyr = f_lam(Z,WyrF), !, eval(WyrF, Base, WynF), 
			Wyn = clause(head(_, [Z]), WynF,[])
	;	Wyr = constr(X), !, Wyn = constr(X)
	;	Wyr =.. [Op, Wyr1, Wyr2], !, eval(Wyr1, Base, Wyn1), 
			eval(Wyr2, Base, Wyn2), Wyn =.. [Op, Wyn1, Wyn2]
	;	Wyr = var(X), !, 
			(	member(clause(head(Wyr, Par), WyrF, DeklL), Base), 
					eval(WyrF, DeklL, WynF1), eval(WynF1, Base, WynF), 
						Wyn = clause(head(Wyr, Par), WynFv, DeklL)
			;	\+ member(clause(head(Wyr, Par), WyrF, DeklL), Base), Wyn = var(X)
			)
	).
   
/* 2i) - sterowanie (przeszukiwanie drzewa w poszukiwaniu aplikacji) */
oblicz(ExprTree, Val):- 
	(	ExprTree = appl(F, WyrAt), !, oblicz(F, ValF),
			wstaw(WyrAt, ValF, Val), !
	;	ExprTree =.. [Op, ExprTree1, ExprTree2], !, oblicz(ExprTree1, Val1), 
			oblicz(ExprTree2, Val2), Val =.. [Op, Val1, Val2]
	;	Val = ExprTree
	).

/* 2iii) i iv) ukonkretnianie korzenia wyrazenia i wypisanie wyniku */
print_proc(ValT):- 
	ukonkretnij(ValT,Val),
	(	Val = clause(_,_,_), !, atom_codes(Atom, "<function>"), print(Atom)
	;	print(Val)
	), nl.
	
ukonkretnij(Wyr,Val):-
	(	Wyr =.. [Op, Wyr1, Wyr2], !,
		(	member(Op, [<, >, <=, >=, =, /=]), !, ukonkretnij(Wyr1, Val1),
				ukonkretnij(Wyr2, Val2), Funk =.. [Op, Val1, Val2],
				(	Funk,!, atom_codes(True, "True"), Val = constr(True)
				;	atom_codes(False, "False"), Val = constr(False)
				)
		;	member(Op, [+, -, *]), ukonkretnij(Wyr1, Val1), ukonkretnij(Wyr2, Val2), 
				Funk =.. [Op, Val1, Val2], Val is Funk
		)
	;	Wyr = clause(head(_,[]), WyrF, []), !, ukonkretnij(WyrF, ValF), Val = ValF
	;	Wyr = constr(num(Val)), !
	;	Val = Wyr	
	).
	
/* Predykat sterujacy praca interpretera */
process_inpt(Base, New_Base):- 
	atom_codes(Atom, ">"), print(Atom), get_code(C1), 
	(	C1 = -1, !, fail
	;	get_code(C2), InptStr =[C1,C2], get_code(_),
		(	InptStr == ":l", !, read_line_to_codes(user_input, FileName), atom_codes(File, FileName), 
				read_file_to_codes(File, Codes, []), process(Codes, Base, New_Base)
		;	InptStr == ":e", !, read_line_to_codes(user_input, Expression), phrase(lexer(Tokens), Expression), phrase(wyrazenie(Wyr), Tokens), trace, eval(Wyr, Base, Wyn), oblicz(Wyn,Val), print(Val), nl,   
						print_proc(Val), New_Base = Base
		;	InptStr == ":q", !, fail
		; 	InptStr == ":p", !, print(Base), nl,  New_Base = Base
		)
	).
	
/* Predykat zapetlajacy (pozwalajacy na wykonanie wiecej niz jednej instrukcji) dzialanie interpretera */
loop(BB) :- 
	process_inpt(BB, NB), loop(NB).

/* Glowny predykat uruchamiajacy program */
main :- 
	loop([]).
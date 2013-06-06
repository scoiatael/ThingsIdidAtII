:- style_check(-atom).

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
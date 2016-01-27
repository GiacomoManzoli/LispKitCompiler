import Lexer
import Analizzatore


test_all = lex_test

-- #########################
-- Programmi LispKit
-- #########################

-- applica il fattoriale a tutti gli elementi di una lista di interi
a = "letrec FACT = lambda (X) if eq(X,0) then 1 else X*FACT(X-1) and " ++
    "G = lambda (H L) if eq(L,nil) then L else cons(H(car(L)),G(H,cdr(L))) " ++
    "in G(FACT,cons(1,cons(2,cons(3,nil)))) end $";

b = "let x = cons(\"ab\",cons(\"cd\",nil)) in if true then cons(\"01\",x) " ++
    "else nil end $";

c = "let x = 5 and y = 6 in x*3 + y*2*x + x*y end $"

-- mostra come si possa utilizzare letrec anche con binder non-funzionali (le
-- variabili a sinistra non devono apparire a destra)
d = "let z = 2 in letrec x = 2+z and y = 2*z in x*y*z end end $"

-- considera una lista di liste Z e produce una lista semplice che contiene
-- tanti interi quante sono le liste contenute in Z e l'intero corrispondente ad
-- una lista contenuta in Z è la somma dei fattoriali dei suoi elementi: f2=fattoriale, f1=calcola somma dei fattori--ali degli elementi di una 
--lista di interi e f0 distribuisce f1 sulle liste contenute in Z *)
e = "letrec f0 = lambda (x) letrec f1 = lambda(y) letrec f2 = lambda (z) " ++
    "if eq(z,1) then 1 else z*f2(z-1) in if eq(y,nil) then 0 else " ++
    "f2(car(y)) + f1(cdr(y)) end in if eq(x,nil) then nil else " ++
    "cons(f1(car(x)),f0(cdr(x))) end in " ++
    "f0(cons(cons(3,cons(3,nil)),cons(cons(3,nil),nil))) end $"

-- mostra un esempio di funzione che restituisce una funzione locale
f = "let "++
        "f1 = lambda () "++
                    "letrec f2 = lambda (z) if eq(z,1) then 1 " ++
                                            "else z*f2(z-1) "++
                    "in f2 "++
                    "end "++
    "in "++
        "let x = f1() "++
        "in x(8) "++
         "end "++
    "end $"

-- funzioni farlocche

g = "let f1 = lambda () let x = 2 in x end in f1 end $"

h = "let "++
        "f1 = lambda () "++
                    "letrec f2 = lambda (z) if eq(z,1) then 1 " ++
                                            "else *f2(z-1) "++
                    "in f2 "++
                    "end "++
    "in "++
        "f1 "++
    "end $"

-- ###########################
-- Test Analizzatore lessicale
-- ###########################

-- test1 per la lettura corretta dei numeri
lex_test1 = ("123 123 ~123 $", [(Number 123),(Number 123), (Number (-123)), (Symbol DOLLAR)])

-- test2 per la lettura corretta dei simboli
lex_test2 = (" = + ( ) $",[Symbol EQUALS,Symbol PLUS,Symbol LPAREN,Symbol RPAREN,Symbol DOLLAR])

-- test3 per la lettura di una keyword o variabile
lex_test3 = (" let x $", [Keyword LET,Id "x",Symbol DOLLAR])

-- test4 per la lettura di un'assegnazione completa
lex_test4 = (" let x = 4 $", [Keyword LET,Id "x",Symbol EQUALS,Number 4,Symbol DOLLAR])
  
-- test5 "falsetto"
lex_test5 = ("falsetto $",[Id "falsetto",Symbol DOLLAR])

-- test6 costante stringa
lex_test6 = ("\"Ciao mamma\" $", [String "Ciao mamma",Symbol DOLLAR])

lex_test = lexi (fst lex_test1)  == snd lex_test1 &&
           lexi (fst lex_test2)  == snd lex_test2 &&
           lexi (fst lex_test3)  == snd lex_test3 &&
           lexi (fst lex_test4)  == snd lex_test4 &&
           lexi (fst lex_test5) == snd lex_test5 &&
           True

-- #####################################
-- Test Analizzatore sintattico semplice
-- #####################################


pars_test1 = (a,Return [Symbol DOLLAR])
pars_test2 = (b,Return [Symbol DOLLAR])
pars_test3 = (c,Return [Symbol DOLLAR])
pars_test4 = (d,Return [Symbol DOLLAR])
pars_test5 = (e,Return [Symbol DOLLAR])
pars_test6 = (f,Return [Symbol DOLLAR])
pars_test7 = (g,Return [Symbol DOLLAR])
pars_test8 = (h,Return [Symbol DOLLAR])

pars_test = parse (fst pars_test1) == snd pars_test1 &&
            parse (fst pars_test2) == snd pars_test2 &&
            parse (fst pars_test3) == snd pars_test3 &&
            parse (fst pars_test4) == snd pars_test4 &&
            parse (fst pars_test5) == snd pars_test5 &&
            parse (fst pars_test6) == snd pars_test6 &&
            parse (fst pars_test7) == snd pars_test7 &&
            parse (fst pars_test8) == snd pars_test8 &&
            True


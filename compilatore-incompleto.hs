--Compilatore LKC --> SECD incompleto Dicembre  2015
module Compilatore(
comp_one,
Secdexpr(..)
)
where

import Analizzatore_sint_2


data Secdexpr = Add | Sub |  Mult | Div | Rem | Eq | Leq | 
                Car | Cdr | Cons | Atom | Join | Rtn | Stop | Push |
                Ap | Rap | Ld (Integer, Integer) |
                Ldc LKC|
                Sel [Secdexpr] [Secdexpr]|
                Ldf [Secdexpr]
                deriving(Show, Eq)


-- ############ Funzioni per il calcolo dell'indirizzo di una variabile nell'ambiente 
position::String -> [LKC]-> Integer
position x [] = error "position: non trova la variabile"
position x ((VAR z):y) = if z==x then 0 else 1 + (position x y)
position x _ = error "position: trovata non VAR"

{-
    Ritorna True se la variabile x è all'interno del record di attivazione [LKC]   
-}
member::String ->[LKC]->Bool
member x [] = False 
member x ((VAR z):y) = if x == z then True else member x y
member x _ = error ("member: trovata non VAR"++ x)

{-
    String è la variabile che si sta cercando
    Integer è il contatore dei record di attivazione passati
    [[LKC]] è lo stack dei record di attivazione
-}
location:: String->Integer-> [[LKC]]-> (Integer, Integer) 
location x _ [] = error ("location non trova VAR"++ x)
{-
   Se la variabile è dentro il record di attivazione corrente (member x n) recupera la posizione utilizzando (position x n)
   altrimenti continua la ricerca sul record di attivazione successivo
-}
location x ct (n:m) =   if (member x n) then (ct, (position x n)) else  (location x (ct+1) m)
 

sexpr_reverse::[a]->[a]
sexpr_reverse []= []
sexpr_reverse (a:b)= (sexpr_reverse b) ++ [a]


--togliere variabili / espressioni da una lista di Binders
vars::[(a,b)]->[a]
vars []= [] 
vars ((x,y):r)= (x : (vars r))

exprs::[(a,b)]->[b]
exprs []= [] 
exprs((x,y):r)= (y:(exprs r))

{-
    Compila una lista di parametri attuali
    [LKC] lista dei parametri da compilare
    n:[[LKC]] ambiente statico
    c: parte già compilata
-}
complist:: [LKC]-> [[LKC]] -> [Secdexpr]->[Secdexpr]
-- Se la lista è vuota aggiunge prima di c NIL
complist [] _ c = ((Ldc NIL):c) 
{-
    (Cons:c) viene eseguito quando in cima alla pila c'è l'ultimo parametro attuale,
    esegue quindi la concatenazione producendo un'unica lista di parametri

    (Comp x n ...) produce il codice per calcolare il parametro attuale e mettere
    il risultato in cima alla pila

    complist y n ... fa l'invocazione ricorsiva

    es: y = [p1,p2]
    passo 1: (comp p1) (cons) c
    passo 2: (comp p2) (cons) (comp p1) (cons) c
    passo 3: (Ldc NIL) (comp p2) (cons) (comp p1) (cons) c
    
    nb: cons è l'istruzione SECD che concatena una lista

    Può essere che un parametro attuale sia una definzione lambda, in questo caso
    nella lista dei parametri viene messa l'istruzione (Ldf ...)
    La lista sarà quindi qualcosa tipo:
    [(Ldc NIL), (Ldf ...), (Cons), (Ld (n1,n2)), (Cons)]
    Quando il codice viene eseguito carica nello stack la chiusura della funzione,
    la concatena a NIL, mette in cima allo stack il valore della variabile ed esegue un
    altro Cons per produrre la lista dei valori da usare per invocare la funzione.

    La differenza sta che i parametri attuali che sono valori, vengono semplicemente caricati
    mentre per quanto riguarda quelli funzionali viene compialto il codice e prodotta la chiusura.
    Da notare che il codice compilato non viene eseguito.

-}
complist (x:y) n c = complist y n (comp x n (Cons:c))

-- ####### FUNZIONE PER LA COMPILAZIONE ########

-- [[LKC]] contiene solo nomi di variabili
-- Il codice viene prodotto da destra a sinistra e viene eseguito da sinistra a destra
comp:: LKC -> [[LKC]] -> [Secdexpr]->[Secdexpr]
comp e n c =  case e of (VAR x) -> ((Ld (location x 0 n)):c)
                        (NUM x)-> (Ldc (NUM x)):c 
                        (BOO x)-> (Ldc (BOO x)):c  
                        (STRI x)-> (Ldc (STRI x)):c 
                        NIL -> (Ldc NIL):c 
                        (ADD x y)-> comp y n (comp x n (Add:c)) -- produce [CodicePerY][CodicePerX][ADD][c]
                        (SUB x y)-> comp y n (comp x n (Sub:c))
                        (MULT x y)-> comp y n (comp x n (Mult:c))
                        (DIV x y)-> comp y n (comp x n (Div:c))
                        (REM x y)-> comp y n (comp x n (Rem:c))
                        (EQC x y)-> comp y n (comp x n (Eq:c))
                        (LEQC x y)-> comp y n (comp x n (Leq:c))
                        (CARC x)-> comp x n (Car:c)  
                        (CDRC x)-> comp x n (Cdr:c)  
                        (CONSC x y)-> comp y n (comp x n (Cons:c))  
                        (ATOMC x)-> comp x n (Atom:c)   
                        (IFC x y z)-> let thenp=(comp y n [Join]) 
                                          elsep=(comp z n [Join]) 
                                      in comp x n  ((Sel thenp elsep):c)
                        {-
                            LAMBDAC x y
                            x sono i parametri formali della funzione
                            y è il corpo della funzione
                            Il compilatore quindi produce l'istruzione per creare la chiusura della funzione (Ldf)
                            Il codice della funzione viene ottenuto compilando y in un nuovo ambiente statico contente
                            i parametri formali.
                            Il codice prodotto termina con un Rtn per far riprendere l'esecuzione del programma
                        -}
                        (LAMBDAC x y)-> (Ldf (comp y (x:n) [Rtn])):c 
                        {-
                            CALL x y
                            y rappresenta i parametri attuali
                            x è il nome della funzione da invocare
                            
                            (complist y n ...) compila le istruzioni per il calcolo dei parametri attuali

                            (comp x n ...) compila l'istruzione che carica in cima allo stack la chiusura
                            della funzione x

                            Ap è l'istruzione che invoca la funzione, in cima allo stack deve esserci una chiusura
                            con sotto la lista dei parametri attuali
                        -}
                        (CALL x y)-> complist y n (comp x n (Ap:c))
                        {-
                            LETC x y

                            [codice per la lista dei parametri attuali (parte destra dei binders), prodotta con complist][(Ldf body)][(Ap)]

                            Da notare che l'ambiente statico quando viene eseguita la funzione deve essere ((binders):n), dove binders è il record definito dalla valutazione della lista.

                            Mentre durante la compilazione della parte destra dei binders l'ambiente deve essere solo n, ovvero
                            deve contenere solo l'ambiente definito precedentemente (questo non è più valido per LETRECC)
                        -}
                        (LETC x y)->  --DA FARE
                        
                        {-
                            LETRECC x y

                            Il codice prodotto deve prima costruire le parti destre dei binder, caricare la chiusura ed infine aggiungere l'istruzione (Rap) al posto di (Ap).

                            A differenza di LETC, quando viene compilata la parte relativa a binders l'ambienete statico deve contenere il record dei binders.

                            Rap viene utilizzato per aggiornare l'ambiente di esecuzione della chiusura della funzione definita, questo perché, dal momento che la funzione è ricorsiva, il valore della chiusura deve essere presente all'interno dell'ambiente nel quale viene eseguita la chisura.
                            Si riesce ad implementare questa cosa in Haskell grazie alla Lazy evaluation.

                            Per evitare problemi nel calcolo degli indirizzi, prima di inserire il codice per calcolare i parametri attuali, è necessario inserire l'istruzione (Push) che crea un RA farlocco [OGA] che permette al compilatore di calcolare gli indirizzi corretti nel caso la parte destra di un binder utilizzi delle variabili presenti nella parte sinistra del binder. 
                            Ovvero:
                                x = z + y
                                w = x + 2
                            Tuttavia, se il binder definisce una funzione, questa può utilizzare dei binder sinistri dal momento che l'ambiente di esecuzione della funzione è diverso.

                        -}
                        (LETRECC x y)-> --DA FARE
                        _ -> [];

 
--esempi di prova

--c="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else c--ons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"



--d= "let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"

comp_one x = comp x [] []
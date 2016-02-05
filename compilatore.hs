--Compilatore LKC --> SECD incompleto Dicembre  2015
module Compilatore(
compile,
compile_string,
Secdexpr(..)
)
where

import Lexer
import Analizzatore


-- ############################################################################################
--  Area per la definzione dei datatype
-- ############################################################################################

-- Datatype che rappresenta le operazioni che la macchina SECD è in grado di compiere
-- Sub, Mul, Div, Rem, Eq, Leq, Car, Cdr, Cons, Atom, Join, Rtn, Ap, Stop, Rap, Push
-- sono tutte operazioni 0-arie che utilizzano gli operandi presenti nello stack
data Secdexpr = Add 
              | Sub 
              | Mult 
              | Div 
              | Rem -- ??
              | Eq 
              | Leq 
              | Car 
              | Cdr 
              | Cons 
              | Atom 
              | Join -- Ricarica il codice dal dump 
              | Rtn 
              | Stop 
              | Push 
              | Ap 
              | Rap 
              | Ld (Integer, Integer)
              | Ldc LKC
              | Sel [Secdexpr] [Secdexpr]
              | Ldf [Secdexpr]
  deriving(Show, Eq)
{-
  Ld (n1:Integer, n2:Integer): carica in cima alla pila S l'n2-esimo valore della n1-esima cella di E
  Ldc LKC: carica in cima alla pila un valore LKC costante (NUM, BOO, STRI, NIL)
  Sel [Secdexpr] [Secdexpr]: compilazione di un if, le due espressioni sono i due rami
  Ldf [Secdexpr]: costruisce in cima alla pila S la chiusura di una funzione


  Join: S E (Join) (C D)-->S E C D
-}

-- ############################################################################################
--  Area per le funzioni d'utilità
-- ############################################################################################

-- position: calcola l'indirizzo di una variabile all'interno dell'ambiente
--  String: nome della variabile
--  [LKC]: ambiente in cui cercare
position :: String -> [LKC]-> Integer
position x [] = error "position: non trova la variabile"
position x ((VAR z):y) = if z==x then 0 else 1 + (position x y)
position x _ = error "position: trovata non VAR"

-- member: verifica se la variabile compare all'interno del RA
--  Stirng: Nome della variabile
--  [LKC]: Record di attivaziome
member :: String -> [LKC] -> Bool
member x [] = False 
member x ((VAR z):y) = if x == z then True else member x y
member x _ = error ("member: trovata non VAR"++ x)

-- location: funzione che ritorna la posizione (n1,n2) della variabile all'interno dello stack
--  String x: è il nome delle variabile
--  Integer ct: è il contatore dei record passati, alla prima invocazione è 0
--  [[LKC]] (n:m): è lo stack
location:: String -> Integer -> [[LKC]] -> (Integer, Integer) 
location x _ [] = error ("location non trova VAR"++ x)
{-
   Se la variabile è dentro il record di attivazione corrente (member x n) recupera la posizione utilizzando (position x n)
   altrimenti continua la ricerca sul record di attivazione successivo
-}
location x ct (n:m) = if (member x n) then (ct, (position x n)) 
                                      else  (location x (ct+1) m)
 
-- sexpr_reverse: inverte gli elemnti di una lista
sexpr_reverse :: [a] -> [a]
sexpr_reverse []= []
sexpr_reverse (a:b)= (sexpr_reverse b) ++ [a]

-- vars: funzione che toglie le espressioni da una lista di Binders
--  es: [(VAR "x", num 1),(VAR "y", num 2),...] --> [VAR "x",VAR "y",...]
vars::[(a,b)]->[a]
vars []= [] 
vars ((x,y):r)= (x : (vars r))

-- exprs: funzione che toglie le espressioni da una lista di Binders
--  es: [(VAR "x", num 1),(VAR "y", num 2),...] --> [NUM 1,NUM 2,...]
exprs::[(a,b)]->[b]
exprs []= [] 
exprs((x,y):r)= (y:(exprs r))

{-
    Compila una lista di parametri attuali
    [LKC] lista dei parametri da compilare
    n:[[LKC]] ambiente statico
    c: parte già compilata
-}
complist :: [LKC] -> [[LKC]] -> [Secdexpr] -> [Secdexpr]
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

-- ############################################################################################
--  Funzione per la compilazione
-- ############################################################################################


-- comp: funzione principale che esegue la compialzione del codice.
-- Il codice viene prodotto da destra a sinistra e viene eseguito da sinistra a destra
-- Parametri:
-- * ist::LKC:        istruzione LKC da compilare
-- * n::[[LKC]]:    ambiente statico, ovvero la lista di liste di nomi di variabili
-- * c::[Secdexpr]: codice finora compilato
comp :: LKC -> [[LKC]] -> [Secdexpr]->[Secdexpr]
comp ist n c = case ist of 
              (VAR x) -> ((Ld (location x 0 n)):c)  -- carica sullo stack il valore della variabile
              (NUM x)-> (Ldc (NUM x)):c -- carica sullo stack il valore constante
              (BOO x)-> (Ldc (BOO x)):c  
              (STRI x)-> (Ldc (STRI x)):c 
              NIL -> (Ldc NIL):c 
              (ADD x y)-> comp y n (comp x n (Add:c)) -- produce [CodicePerY][CodicePerX][ADD][c] in questo modo quando viene eseguita l'add in cima alla pila c'è il valore di X
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
                            in comp x n ((Sel thenp elsep):c)
              {-
                  LAMBDAC x y
                  x sono i parametri formali della funzione
                  y è il corpo della funzione
                  Il compilatore quindi produce l'istruzione per creare la chiusura della funzione (Ldf)
                  Il codice della funzione viene ottenuto compilando y in un nuovo ambiente statico contente i parametri formali.
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

                  Ap è l'istruzione che invoca la funzione, in cima allo stack deve esserci una chiusura con sotto la lista dei parametri attuali
              -}
              (CALL x y) -> complist y n (comp x n (Ap:c))
              {-
                  LETC x y

                  [codice per la lista dei parametri attuali (parte destra dei binders), prodotta con complist][(Ldf body)][(Ap)]

                  Da notare che l'ambiente statico quando viene eseguita la funzione deve essere ((binders):n), dove binders è il record definito dalla valutazione della lista.

                  Mentre durante la compilazione della parte destra dei binders l'ambiente deve essere solo n, ovvero
                  deve contenere solo l'ambiente definito precedentemente (questo non è più valido per LETRECC)
              -}
              (LETC x y) -> let
                              e = exprs y -- espressioni per il valore dei binders
                              v = vars y  -- definizioni dei binders (nomi)
                              comp_body = comp x (v:n) [Rtn] 
                              -- comp_body è la compilazione del codice della parte IN
                              -- viene fatta con il record delle variabil in cima il all'ambiente 
                              -- dinamico 
                            in
                              complist e n ((Ldf comp_body):Ap:c)
                              -- complist e n produce il codice per calcolare il valore dei binders
                              -- Ldf costruisce la chiusura per la parte IN (codice prodotto da comp_body)
                              -- Ap esegue la chiusura che si trova in cima alla pila
                              -- c è il codice finora prodotto.
              
              {-
                  LETRECC x y

                  Il codice prodotto deve prima costruire le parti destre dei binder, caricare la chiusura ed infine aggiungere l'istruzione (Rap) al posto di (Ap).

                  A differenza di LETC, quando viene compilata la parte relativa a binders l'ambienete statico deve contenere il record dei binders.

                  Rap viene utilizzato per aggiornare l'ambiente di esecuzione della chiusura della funzione definita, questo perché, dal momento che la funzione è ricorsiva, il valore della chiusura deve essere presente all'interno dell'ambiente nel quale viene eseguita la chisura.
                  Si riesce ad implementare questa cosa in Haskell grazie alla Lazy evaluation.

                  Per evitare problemi nel calcolo degli indirizzi, prima di inserire il codice per calcolare i parametri attuali, è necessario inserire l'istruzione (Push) che crea un RA farlocco [OGA] che permette al compilatore di calcolare gli indirizzi corretti nel caso la parte destra di un binder utilizzi delle variabili presenti nella parte sinistra del binder. 
                  
                  Tuttavia, se il binder definisce una funzione, questa può utilizzare dei binder sinistri dal momento che l'ambiente di esecuzione della funzione è diverso.

              -}
              (LETRECC x y)-> let
                                e = exprs y -- espressioni per il valore dei binders
                                v = vars y  -- definizioni dei binders (nomi)
                                comp_body = comp x (v:n) [Rtn] 
                                -- comp_body è la compilazione del codice della parte IN
                                -- viene fatta con il record delle variabil in cima il all'ambiente 
                                -- statico 
                              in
                                Push:(complist e (v:n) ((Ldf comp_body):Rap:c))
                                -- Push inserisce il record farlocco
                                -- complist e v:n produce il codice per calcolare il valore dei binders, l'ambiente statico di complist contiene in cima anche il nome dei binders, questo perché le espressioni dei binders possono essere delle definizioni di funzioni ricorsive e di conseguenza devono avere a disposizione anche il loro nome che è contenuto in v.
                                -- La presenza di v nell'ambiente statico porta un problema nel calcolo degli indirizzi, perché viene considerato un RA in più, che a runtime non è presente. Per questo motivo viene prima eseguita una PUSH (che non modifica l'ambiente statico) la quale aggiunge un RA fittizzio che rappresenta l'RA v dell'ambiente dinamico.
                                -- Ldf costruisce la chiusura per la parte IN (codice prodotto da   comp_body)
                                -- Ap esegue la chiusura che si trova in cima alla pila
                                -- c è il codice finora prodotto.
              _ -> [];

compile :: LKC -> [Secdexpr]
compile x = comp x [] []

compile_string :: String -> [Secdexpr]
compile_string x = compile (parse x)
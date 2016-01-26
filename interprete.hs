--INTERPRETE SECD COMPLETO in Haskell
module Interprete(
interprete,
Valore(..),
) where

import Compilatore
import Analizzatore_sint_2

--tipo che modella gli R-valori delle variabili. Si tratta dei valori da mettere nella pila S e nell'ambiente dinamico E. In 
--particolare CLO modella le chiusure.  *)

--OGA= place holder per l'ambiente *)
{-
  V LKC - rappresenta i valori "normali", interi, booleani, stringhe...
  OGA   - record di attivazione farlocco
  CLO [Secdexpr] [[Valore]] - Chiurusa, il primo parametro è il codice della funzione, mentre il secondo contiene la copia dell'ambiente di esecuzione 
  VLISTA [Valore] - Rappresenta una lista di parametri che può trovarsi in cima alla pila
-}
data Valore = V  LKC| OGA |
              CLO [Secdexpr] [[Valore]]| 
              VLISTA [Valore]
              deriving(Show,Eq)

--datatype dei valori del Dump *)
{-
  TRIPLA è il datatype per i dati da tenere nel dump
  CONR viene utilizzato per gli if-then-else
-}
data Dump = CONTR  [Secdexpr] | 
            TRIPLA [Valore][[Valore]][Secdexpr] | DUMMY
            deriving(Show,Eq)



--funzione che crea l'ambiente dinamico ricorsivo necessario per il trattamento della ricorsione. Serve nel caso Rap
{-
  lazyE viene invocata con due volta la stessa lista dei parametri 
-}
lazyE::[Valore]-> [Valore]->[Valore] 
lazyE [] _ = []
{-
  Se la lista non è vuota esegue lazyClo sull'elemento in testa, dopodiché continua ricorsivamente sui parametri successivi
-} 
lazyE (a:b) c = ((lazyClo a c):(lazyE b c))

lazyClo:: Valore -> [Valore] -> Valore
{- 
  Caso con una chiusura.
  Viene creata una nuova chiusura con lo stesso body e come ambiente lo stesso ambienete con in cima la lista dei parametri resa ricorsiva.

  Potrebbe esserci un errore in questa funzione
-}
lazyClo (CLO a b) c = (CLO a ((lazyE c c):b))
lazyClo (V x) _= (V x) 
lazyClo (VLISTA x) _= (VLISTA x)
lazyClo x _= error ("LazyClo trova valore incompatibile" ++ (show x))


--funzioni per la ricerca degli R-valori dati i loro indirizzi: usate da Ld *)

index::Integer ->[a]->a
index n  s= if n==0 then (head s) else (index (n-1) (tail s))  

locate::(Integer, Integer)-> [[Valore]]->Valore
locate  (a,b)  e = (index b (index a e));
      
extract_int (V (NUM x)) = x 
extract_int x = error ("trovato altro da intero" ++ (show x))

--funzioni per le liste di Valori VLISTA 

vhd (VLISTA (a:b)) = a 
vhd (VLISTA [])  = error "vhd trovata lista vuota"
vhd _ = error "vhd non trova VLISTA"

vtl (VLISTA (a:b)) = VLISTA b  
vtl (VLISTA [])  = error "vtl trovata lista vuota";
vtl _ = error "vtl non trova VLISTA"

vatom (V k)= V (BOO True)
vatom _ = V (BOO False)     
                            
bool2s_espressione:: Bool ->LKC            
bool2s_espressione b = if b then (BOO True) else (BOO False)

--test di uguaglianza per il tipo Valore, si adatta ai tipi dei parametri con cui è invocata
eqValore::Valore -> Valore -> Bool
eqValore a@(V _) b = (eqV a b)
eqValore a@(VLISTA _) b = (eqVLISTA a b)
eqValore a  b = error ("uguaglianza tra chiusure"++ (show a) ++ (show b))

eqVLISTA::Valore -> Valore ->Bool
eqVLISTA (VLISTA []) (VLISTA [])= True 
eqVLISTA (VLISTA(a:b)) (VLISTA (c:d)) = (eqValore a c) && (eqVLISTA (VLISTA b) (VLISTA d))
eqVLISTA _ _= False


eqV (V a) (V b)= a==b
eqV _ _= False

 
--FUNZIONE PRINCIPALE
{-
  s - Stack
  e - Ambiente dinamico, sono i record di attivazione
  c - Controllo, il programma in esecuzione
  d - Dump
-}
interprete:: [Valore] -> [[Valore]]-> [Secdexpr]-> [Dump]-> Valore
interprete s e c d = case (head c) of 
                    Ld(b, n) -> let x = (locate (b,n) e)  in (interprete (x:s) e (tail c) d)

                    (Ldc k) -> case k of NIL -> (interprete ((VLISTA []):s) e (tail c) d)
                             _ -> (interprete ((V k):s) e (tail c) d)

                    Add -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                           in  (interprete ((V(NUM (operand1 + operand2))):(tail (tail s))) e (tail c)  d)
                    
                    Sub -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                           in  (interprete ((V(NUM (operand1 - operand2))):(tail (tail s))) e (tail c)  d)

                    Mult -> let operand1 = extract_int (head s)
                                operand2 = extract_int(head (tail s))
                            in  (interprete ((V(NUM (operand1 * operand2))):(tail (tail s))) e (tail c)  d)
                    
                    Div -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                           in  (interprete ((V(NUM (operand1 `div` operand2))):(tail (tail s))) e (tail c)  d)
                    
                    Rem -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                           in  (interprete ((V(NUM (operand1 `mod` operand2))):(tail (tail s))) e (tail c)  d)
                    
                    Leq -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                           in  (interprete ((V(bool2s_espressione (operand1 <= operand2))):(tail (tail s))) e (tail c)  d)
                    
                    Eq -> case s of (w1:w2:w3) -> (interprete ((V (bool2s_espressione (eqValore w1 w2))):w3) e (tail c) d)
                        _-> error "manca un argomento in Eq"
                    
                    Car -> (interprete ((vhd(head s) ):(tail s)) e (tail c) d)
                    
                    Cdr -> (interprete ((vtl(head s) ):(tail s)) e (tail c) d)
                    
                    Cons -> case head (tail s) of (VLISTA x)-> (interprete  (VLISTA ((head s):x):(tail (tail s))) e (tail c) d)
                                                  x-> error ("CONS: il secondo argomento non e' una lista" ++ (show  x))
                    
                    Atom ->  (interprete ((vatom (head s)):(tail s)) e (tail c) d)

                    Sel sl1 sl2 -> case head s of (V (BOO True)) -> (interprete (tail s) e sl1 ((CONTR (tail c)):d))
                                                  (V (BOO False)) -> (interprete (tail s) e sl2 ((CONTR (tail c)):d))
                                                  _ -> error "non c'e' bool su s quando si esegue SEL"

                    
                    Join -> case (head d) of (CONTR c1) -> (interprete s e c1 (tail d))
                                             _ -> error "JOIN: il dump non contiene controllo"
                    {-
                      Costruzione della chiusura di una funzione, viene messo in cima allo stack il valore chiusura con il codice e con la copia dell'ambiente d'esecuzione, l'esecuzione continua dall'istruzione successiva
                    -}
                    Ldf sl -> (interprete ((CLO sl e):s) e (tail c) d)

                    Ap -> case (head s) of 
                            (CLO c1 e1) -> case (head (tail s)) of
                                        VLISTA x -> (interprete [] (x:e1) c1 ((TRIPLA (tail(tail s)) e (tail c)):d))
                                        _  -> error "AP senza lista dei parametri"
                              
                            _  -> error "AP senza chiusura su s"
                    
                    Rtn ->  case (head d) of (TRIPLA s1 e1 c1) -> (interprete ((head s):s1) e1 c1 (tail d))
                                 _ ->  error  "RTN: non trovata TRIPLA su dump"   
                    {-  RAP
                      lo stack deve avere la forma

                        [CLO (body, E)]
                        [VLISTA [p1,p2,...]]
                        [...]

                    -}
                    Rap -> case (head s) of  
                            (CLO c1 e1) ->  case e1 of
                                         {- 
                                            Il primo record di attivazione dell'ambiente della chiusura deve essere un record [OGA]
                                         -}   
                                         ([OGA]:re) -> case (head (tail s)) of
                                                       {-
                                                          Nello stack, sotto la chiusura, deve esserci la VLISTA con i parametri.
                                                          Se c'è viene eseguito il corpo della funzione con:
                                                            - stack vuoto
                                                            - ambiente dinamico con in cima la lista dei parametri e sotto l'ambiente della chiusura. (LazyE ...) fa si che la lista dei parametri diventi circolare e viene messo al posto di OGA. 
                                                            - come controllo c'è il codice della funzione
                                                       -}
                                                        (VLISTA vl2) -> (interprete [] ((lazyE vl2 vl2):re) c1 ((TRIPLA (tail (tail s))( tail e) (tail c)):d))
                                                        _ -> error "manca [OGA] sull'ambiente di chiusura ric"
                                         _ -> error "non trovata [OGA] nell'ambiente di chiusura ricorsiva"
                            _  -> error "RAP: non trovata chiusura su s"

                    Push ->(interprete s  ([OGA]:e)  (tail c)  d)
                    Stop -> (head s)
                    _  -> error "operazione non riconosciuta"

--per facilitare l'uso di interprete		
fin x = (interprete [] [] (x ++ [Stop]) [])  --se x e'  programma Secdexpr da eseguire. Si aggiunge Stop alla fine.

-- Mostra che si puo' usare letrec anche con binders non-funzionali. Le var a sinistra non devono apparire a destra 

e = "let z=2 in letrec x= 2+z and y= 2*z in x*y*z end end $"

-- distribuisce FACT su una lista di interi *)

--val S="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X * FACT(  X - 1 )"^
--" and G = lambda ( H  L ) if  eq ( nil, L ) then L else cons( H(car( L ) ), G ( H , cdr ( L ) ))"^
--" in G ( FACT, cons( 6 ,cons( 7, cons( 8 , nil))) ) end $";

--(*considera liste di liste Z e produce una lista semplice che contiene tanti interi quante sono le liste contenute in Z e l'intero 
--corrispondente ad una lista contenuta in Z è la somma dei fattoriali dei suoi elementi: f2=fattoriale, f1=calcola somma dei fattori--ali degli elementi di una 
--lista di interi e f0 distribuisce f1 sulle liste contenute in Z *)

f="letrec f0 = lambda ( x ) 
          letrec f1 = lambda(y) 
                 letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) 
                 in if eq( y , nil ) then 0 else f2 ( car ( y ) ) + f1 ( cdr (y)) end 
          in if eq(x , nil) then nil else cons (f1 ( car ( x )),f0 ( cdr ( x ) ) ) end 
   in f0( cons (cons (3 , cons (3 , nil)), cons( cons (3 , nil), nil))) end $"


--(* esempio di funzione che restituisce una funzione locale *)

g="let f1 = lambda() letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) in f2 end in let x=f1() in x(8) end end $"


--Tok=lexi(explode S)

--(k1,k2)=PROG(Tok) 





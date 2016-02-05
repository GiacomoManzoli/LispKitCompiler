module Analizzatore(Exc(..), progdoll, parse) where

import Lexer -- importa l'analizzatore lessicale, in particolare la funzione lexi
import Prelude hiding (EQ,exp)


-- ##############################################
--  Area per la definzione dei datatype
-- ##############################################


data Exc a = Raise Exception | Return a
type Exception = String

instance Show a => Show (Exc a) where
 show (Raise e)= "ERRORE:\n\t" ++ e
 show (Return x) = "RAGGIUNTO:\n\t" ++ (show x)

instance Functor Exc where
  fmap f (Raise e) = Raise e 
  fmap f (Return v) = Return (f v)

instance Applicative Exc where  
    pure = Return  
    (Raise e) <*> _ = Raise e
    (Return f) <*> something = fmap f something

instance Monad Exc where
 return x  = Return x
 (Raise e) >>= q   = Raise e
 (Return x) >>= q  = q x 

raise :: Exception -> Exc a
raise e = Raise e

-- ##############################################
--  Area per le funzioni di ricerca
-- rec_qualcosa = funzione che ricerca *qualcosa* nella lista dei token
-- ##############################################

rec_key :: [Token] -> Exc [Token]
rec_key ((Keyword LET):b)    = Return b
rec_key ((Keyword LETREC):b) = Return b 
rec_key (a:b)                = Raise ("trovato " ++ show(a) ++", atteso LET o LETREC")
rec_key  x                   = Raise ("ERRORE STRANO"  ++  show(x))

rec_in :: [Token] -> Exc [Token]
rec_in ((Keyword IN):b) = Return b
rec_in (a:b)            = Raise ("trovato " ++ show(a) ++ ", atteso IN")

rec_end :: [Token] -> Exc [Token]
rec_end ((Keyword END):b) = Return b 
rec_end (a:b)             = Raise ("trovato " ++ show(a) ++ ", atteso END")

rec_then :: [Token] -> Exc [Token]
rec_then ((Keyword THEN):b) = Return b
rec_then (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso THEN")

rec_else :: [Token] -> Exc [Token]
rec_else ((Keyword ELSE):b) = Return b
rec_else (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")

rec_lp :: [Token] -> Exc [Token] -- parentesi sinistra
rec_lp ((Symbol LPAREN):b) = Return b 
rec_lp (a:b)               = Raise ("trovato " ++ show(a) ++ ", atteso (")

rec_rp :: [Token] -> Exc [Token] -- parentesi destra
rec_rp ((Symbol RPAREN):b) = Return b 
rec_rp (a:b)               = Raise ("trovato " ++ show(a) ++ ", attesa )")

rec_virg :: [Token] -> Exc [Token]
rec_virg ((Symbol VIRGOLA):b) = Return  b 
rec_virg (a:b)                = Raise ("trovato " ++ show(a) ++ ", attesa ,")

rec_equals :: [Token] -> Exc [Token]
rec_equals ((Symbol EQUALS):b) = Return b 
rec_equals (a:b)               = Raise ("trovato " ++ show(a) ++ ", atteso =")

-- ##############################################
--  Area per le funzione d'utilità
-- ##############################################

-- progdoll: data una lista di token che rappresenta un programma, stampa la stringa
-- che lo rappresenta.
-- progdoll sta per PROGram + DOLLaro
progdoll :: [Token] -> String
progdoll x = show (prog x)

-- ##############################################
--  Area per l'analisi sintattica
-- ##############################################

--funzione che ritorna il valore LKC alla fine del programma
-- parse :: String -> Exc [Token]
parse x = prog (lexi x)

{- 
  STRUTTURA DI UN PROGRAMMA LispKit
    letrec 
      FACT = lambda ( X ) if eq ( X, 0 ) then 1 else X*FACT( X - 1 ) 
      and 
      G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) 
    in 
      G ( FACT, cons(1, cons(2, cons(3, nil))) ) 
    end $
-}

{- #################### IMPORTANTE ####################
  Le funzioni di questa area riguardano l'implementazione della tabella di parsing della grammatica.
  I nomi delle funzioni rappresentano il nome del non termiale.
  I vari casi rappresentano le varie produzioni
-}

-- prog: funzione che rappresenta le produzioni del non terminale Prog
-- ​1)   Prog​ ::= let Bind in Exp end 
-- 1.2) Prog​ ::= letrec Bind in Exp end
prog :: [Token] -> Exc [Token]
prog token_list = do
          x <- rec_key token_list -- cerca il token LET o LETREC
          y <- bind x             -- cerca una produzione di Bind
          z <- rec_in y           -- cerca il token IN
          w <- exp z              -- carca una produzione di Exp
          rec_end w               -- cerca il token END


-- bind: funzione che rappresenta le produzioni del non terminale Bind
-- (Id a) è il token che rappresenta il nome del bind
-- 2) Bind​ ::= var = Exp X
bind :: [Token] -> Exc [Token] 
bind ((Id a):b) =  do
                    x <- rec_equals b  -- cerca il token (Symbol EQUALS)
                    y <- exp x         -- cerca l'espressione del binder
                    funx y            -- controlla se è finito il processing dei binders 
bind (a:_)      = Raise ("BINDER CON "++ show(a) ++" A SINISTRA")

-- funx: funzione che rappresenta le produzioni del non terminale X
-- 3) X​ ::= and Bind
funx :: [Token] -> Exc [Token]
funx ((Keyword AND):b)  = bind b   -- processa il binder successivo
funx a@((Keyword IN):b) = Return a -- a causa di 3.2) X ::= Epsilon
                                   -- devo considerare anche il follow
funx (a:_)              = Raise ("DOPO BINDERS; TROVATO"++show(a))
 

-- exp: funzione che rappresenta le produzioni del non terminale Exp
-- 4.1) Exp​ ::= Prog 
exp :: [Token] -> Exc [Token]
exp a@((Keyword LET):b)    = (prog a) -- progessa il blocco LET ... IN ... END
exp a@((Keyword LETREC):b) = (prog a) -- progessa il blocco LETREC ... IN ... END
-- 4.2) Exp​ ::= lambda(Seq_Var) Exp 
-- ATTENZIONE: il codice fornito per questa parte era errato.
exp ((Keyword LAMBDA):b)   = do
                                x <- rec_lp b
                                y <- seq_var x
                                z <- rec_rp y
                                w <- exp z
                                Return w
-- 4.4) Exp​ ::= OPP(Seq_Exp) 
exp ((Operator CONS):b)    = do
                                x <- rec_lp b
                                y <- exp x
                                z <- rec_virg y
                                w <- exp z
                                rec_rp w
exp ((Operator LEQ):b)     = do
                                x <- rec_lp b
                                y <- exp x
                                z <- rec_virg y
                                w <- exp z
                                rec_rp w
exp ((Operator EQ):b)      = do
                                x <- rec_lp b
                                y <- exp x
                                z <- rec_virg y
                                w <- exp z
                                rec_rp w
exp ((Operator CAR):b)      = exp b
exp ((Operator CDR):b)      = exp b
exp ((Operator ATOM):b)     = exp b
-- 4.5) Exp​ ::= if Exp then Exp else Exp
exp ((Keyword IF):b)        = do
                                x <- exp b
                                y <- rec_then x
                                z <- exp y
                                w <- rec_else z
                                exp w
-- 4.3) Exp​ ::= ExpA 
exp x =  expa x

-- expa: funzione che rappresenta le produzioni del non terminale ExpA
-- 5) ExpA​ ::= T E1
expa :: [Token] -> Exc [Token]
expa a = do
           x <- funt a
           fune1 x

-- fune1: funzione che rappresenta le produzioni del non terminale E1
-- 6) E1​ ::= OPA T E1
fune1 :: [Token] -> Exc [Token]
fune1 ((Symbol PLUS):b) = do
                            x <- funt b
                            fune1 x
fune1 ((Symbol MINUS):b) = do
                             x<-funt b
                             fune1 x
fune1 x = Return x -- a causa di 6.2) E1 ::= Epsilon devo considerare anche il follow

-- funt: funzione che rappresenta le produzioni del non terminale T
-- 7) T​ ::= F T1
funt :: [Token] -> Exc [Token]
funt a = do
           x <- funf a
           funt1 x

-- funt1: funzione che rappresenta le produzioni del non terminale T1
-- 8) ​T1​ ::= OPM F T1
funt1 :: [Token] -> Exc [Token]
funt1 ((Symbol TIMES):b) = do
                             x<-funf b
                             funt1 x
funt1 ((Symbol DIVISION):b) = do
                                x<-funf b
                                funt1 x
funt1 x = Return x -- a causa di 8.2) T1 ::= Epsilon devo considerare anche il follow

-- funf: funzione che rappresenta le produzioni del non terminale F
-- 9.2) ​F​ ::= exp_const
funf :: [Token] -> Exc [Token]
funf (a:b) = if (exp_const a) then Return b  
                              else funf_contd (a:b)

-- funf_contd: continuato di funf.
-- 9.1) F ::= var Y
funf_contd :: [Token] -> Exc [Token]
funf_contd ((Id _):b)              = funy b
-- 9.3) F ::= (ExpA)
funf_contd ((Symbol LPAREN):b) = do
                                   x<- expa b
                                   rec_rp x
funf_contd (a:_)               = Raise ("ERRORE in funf_contd, TROVATO"++ show(a))

-- exp_const: funzione d'utilità che ritorna True se il token ricevuto come parametro
-- rappresenta un espressione costante.
exp_const::Token ->Bool
exp_const (Number _) = True
exp_const Nil        = True
exp_const (Bool _)   = True
exp_const (String _) = True 
exp_const  _         = False

-- funy: funzione che rappresenta le produzioni del non terminale Y
-- 10) ​Y​ ::= (Seq_Exp)
funy :: [Token] -> Exc [Token]
funy ((Symbol LPAREN):b)      =  do
                                 x<-seq_exp b
                                 rec_rp x
funy x = Return x -- a causa di 10.2) Y ::= Epsilon

-- seq_exp: funzione che rappresenta le produzioni del non terminale seq_exp
-- 14) ​Seq_Exp​ ::= Exp ; Seq_Exp
seq_exp:: [Token] -> Exc [Token]
seq_exp a@((Symbol RPAREN):b)   = Return a -- a causa di 14.2) Seq_Exp ::= Epsilon
                                           -- devo considerare anche il follow
seq_exp a = do 
              x <- exp a
              y <- exp_separator x
              Return y

-- exp_separtor: unzione che rappresenta le produzioni del non terminale Exp_Separator
-- 15) Exp_Separator ::= , Exp Exp_Separtor
exp_separator:: [Token]-> Exc [Token]
exp_separator ((Symbol VIRGOLA):b) = do
                                    z <- exp b
                                    t <- exp_separator z
                                    Return t
exp_separator a@((Symbol RPAREN):b) = Return a -- a causa di 15.2) Exp_Separator ::= Epsilon
                                               -- devo considerare anche il follow
exp_separator (a:_) =  Raise ("ERRORE in exp_separator, TROVATO "++ show(a) ++" ATTESO )")

-- seq_var: funzione che rappresenta le produzioni del non terminale seq_var
-- 16) Seq_Var​ ::= var Seq_Var
seq_var :: [Token] -> Exc [Token]
seq_var a@((Symbol RPAREN):b) = Return a -- a causa di 16.2) Seq_Var ::= Epsilon
                                         -- devo considerare anche il follow
seq_var ((Id a):b) = do
                       z<-seq_var b
                       Return z
seq_var (a:_) =  Raise ("ERRORE in seq_var, TROVATO "++ show(a))
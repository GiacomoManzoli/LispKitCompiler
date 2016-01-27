module Analizzatore(Exc(..), progdoll, parse) where

import Lexer -- importa l'analizzatore lessicale, in particolare la funzione lexi e il datatype Token
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

-- Datatype per le varie istruzioni LispKitC (LispKit compilato)
-- Viene utilizzato per creare l'albero astratto durante il parsing
data LKC = ETY 
         | VAR String 
         | NUM Integer 
         | BOO Bool 
         | STRI String 
         | NIL 
         | ADD LKC LKC 
         | SUB LKC LKC 
         | MULT LKC LKC 
         | DIV LKC LKC 
         | REM LKC LKC             -- ??
         | EQC LKC LKC 
         | LEQC LKC LKC 
         | CARC LKC                -- head di una lista
         | CDRC LKC                -- tail di una lista
         | CONSC LKC LKC           -- concatenazione di due liste
         | ATOMC LKC 
         | IFC LKC LKC LKC 
         | LAMBDAC [LKC] LKC 
         | CALL LKC [LKC]          -- CALL NomeFunzione [ListParametri] 
         | LETC LKC [(LKC,LKC)]    -- LETC Corpo [(var,exp)] 
         | LETRECC LKC [(LKC,LKC)] -- LETRECC Corpo [(var,exp)]
  deriving (Show,Eq)

-- ##############################################
--  Area per le funzioni di ricerca
-- rec_qualcosa = funzione che ricerca *qualcosa* nella lista dei token
-- ##############################################

rec_key :: [Token] -> Exc ([Token], String)
rec_key ((Keyword LET):b)    = Return (b, "LET")
rec_key ((Keyword LETREC):b) = Return (b, "LETREC") 
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

-- FA COSE
--funzione che ritorna il valore LKC alla fine del programma
-- parse :: String -> Exc [Token]
parse x = prog (lexi x)

-- FA COSE
--estrae il valore LKC dalla monade finale
--extractLKC :: Exc ([Token], LKC) -> LKC
--extractLKC (Return (a,b)) = b
--extractLKC (Raise x) = error x

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

-- ############################################################################
-- prog: funzione che rappresenta le produzioni del non terminale Prog
-- ​1)   Prog​ ::= let Bind in Exp end 
-- 1.2) Prog​ ::= letrec Bind in Exp end
-- Bisogna produrre:
-- LETC LKC [(LKC,LKC)]    -- LETC Corpo [(var,exp)] 
-- LETRECC LKC [(LKC,LKC)] -- LETRECC Corpo [(var,exp)]
prog :: [Token] -> Exc ([Token], LKC)
prog token_list = do
          (x, tipo_let) <- rec_key token_list -- cerca il token LET o LETREC
          (y, trad_bind) <- bind x            -- cerca una produzione di Bind, produce anche la traduzione
          z <- rec_in y           -- cerca il token IN
          (w, trad_exp) <- exp z              -- carca una produzione di Exp, produce anche la traduzione
          k <- rec_end w               -- cerca il token END
          -- ritorna la lista di token e la traduzione
          Return (k, if (tipo_let == "LET") then LETC trad_exp trad_bind
                                            else LETRECC trad_exp trad_bind)

-- ############################################################################
-- bind: funzione che rappresenta le produzioni del non terminale Bind
-- (Id a) è il token che rappresenta il nome del bind
-- 2) Bind​ ::= var = Exp X
-- Bisogna produrre:
-- [(VAR ‘‘x’’,NUM 5),(VAR ‘‘y’’,NUM 6)]
bind :: [Token] -> Exc ([Token], [(LKC, LKC)])
bind ((Id a):b) =  do
                    x <- rec_equals b         -- cerca il token (Symbol EQUALS)
                    (y, trad_exp) <- exp x    -- cerca l'espressione del binder
                    (w, trad_resto) <- funx y -- controlla se è finito il processing dei binders
                    Return (w, [(VAR (a),trad_exp)]++trad_resto)
bind (a:_)      = Raise ("BINDER CON "++ show(a) ++" A SINISTRA")

-- ############################################################################
-- funx: funzione che rappresenta le produzioni del non terminale X
-- 3) X​ ::= and Bind
funx :: [Token] -> Exc ([Token], [(LKC, LKC)])
funx ((Keyword AND):b)  = bind b         -- processa il binder successivo
funx a@((Keyword IN):b) = Return (a, []) -- a causa di 3.2) X ::= Epsilon
                                         -- devo considerare anche il follow
funx (a:_)              = Raise ("DOPO BINDERS; TROVATO"++show(a))
 
-- ############################################################################
-- exp: funzione che rappresenta le produzioni del non terminale Exp
-- 4.1) Exp​ ::= Prog 
exp :: [Token] -> Exc ([Token], LKC)
exp a@((Keyword LET):b)    = (prog a) -- progessa il blocco LET ... IN ... END
exp a@((Keyword LETREC):b) = (prog a) -- progessa il blocco LETREC ... IN ... END
-- 4.2) Exp​ ::= lambda(Seq_Var) Exp 
-- ATTENZIONE: il codice fornito per questa parte era errato.
-- Bisogna produrre: LAMBDAC [LKC] LKC 
exp ((Keyword LAMBDA):b)   = do
                                x <- rec_lp b
                                (y, trad_seq_var) <- seq_var x
                                z <- rec_rp y
                                (w, trad_exp) <- exp z
                                Return (w, LAMBDAC trad_seq_var trad_exp)
-- 4.4) Exp​ ::= OPP(Seq_Exp) 
-- Bisogna produrre: CONSC LKC LKC 
exp ((Operator CONS):b)    = do
                                x <- rec_lp b
                                (y, trad_exp) <- exp x
                                z <- rec_virg y
                                (w, trad_exp_2) <- exp z
                                r <- rec_rp w
                                Return (r, CONSC trad_exp trad_exp_2)
-- Bisogna produrre: LEQC LKC LKC                                
exp ((Operator LEQ):b)     = do
                                x <- rec_lp b
                                (y, trad_exp) <- exp x
                                z <- rec_virg y
                                (w, trad_exp_2) <- exp z
                                r <- rec_rp w
                                Return (r, LEQC trad_exp trad_exp_2)
-- Bisogna produrre: EQC LKC LKC                          
exp ((Operator EQ):b)      = do
                                x <- rec_lp b
                                (y, trad_exp) <- exp x
                                z <- rec_virg y
                                (w, trad_exp_2) <- exp z
                                r <- rec_rp w
                                Return (r, EQC trad_exp trad_exp_2)
-- Bisogna produrre: CARC LKC                                
exp ((Operator CAR):b)      = do
                                (z, trad_exp) <- exp b
                                Return (z, CARC trad_exp)
-- Bisogna produrre: CDRC LKC 
exp ((Operator CDR):b)      = do
                                (z, trad_exp) <- exp b
                                Return (z, CDRC trad_exp)
-- Bisogna produrre: ATOMC LKC 
exp ((Operator ATOM):b)     = do
                                (z, trad_exp) <- exp b
                                Return (z, ATOMC trad_exp)
-- 4.5) Exp​ ::= if Exp then Exp else Exp
-- Bisogna produrre: IFC LKC LKC LKC
exp ((Keyword IF):b)        = do
                                (x, trad_exp) <- exp b
                                y <- rec_then x
                                (z, trad_exp_2) <- exp y
                                w <- rec_else z
                                (r, trad_exp_3) <- exp w
                                Return (r, IFC trad_exp trad_exp_2 trad_exp_3)
-- 4.3) Exp​ ::= ExpA 
exp x =  expa x

{- ########## IMPORTANTE ##########
  Le funzioni per le produzioni 5,6,7,8 devono essere modificate
  Questo perché per tradurre correttamente un'espressione algebrica con E1
  è necessario sapere cosa ha traddoto ExpA.
  E1 deve quindi essere in grado di ricevere la traduzione fatta da ExpA
  come parametro.
  Lo stesso vale per T e T1
-}

-- ############################################################################
-- expa: funzione che rappresenta le produzioni del non terminale ExpA
-- 5) ExpA​ ::= T E1
expa :: [Token] -> Exc ([Token], LKC)
expa a = do
           (x, trad_t) <- funt a
           fune1 x trad_t -- passa a fune1 anche la traduzione

-- ############################################################################
-- fune1: funzione che rappresenta le produzioni del non terminale E1
-- 6) E1​ ::= OPA T E1
-- Bisogna produrre
--  ADD LKC LKC 
--  SUB LKC LKC 
fune1 :: [Token] -> LKC -> Exc ([Token], LKC)
fune1 ((Symbol PLUS):b) po = do
                            (x, trad_t)  <- funt b
                            (y, trad_e1) <- fune1 x trad_t
                            Return (y, ADD po trad_e1)
fune1 ((Symbol MINUS):b) po = do
                             (x, trad_t)  <- funt b
                             (y, trad_e1) <- fune1 x trad_t
                             Return (y, SUB po trad_e1)
fune1 x po = Return (x, po) -- a causa di 6.2) E1 ::= Epsilon devo considerare anche il follow

-- ############################################################################
-- funt: funzione che rappresenta le produzioni del non terminale T
-- 7) T​ ::= F T1
funt :: [Token] -> Exc ([Token], LKC)
funt a = do
           (x, trad_f)  <- funf a
           funt1 x trad_f

-- ############################################################################
-- funt1: funzione che rappresenta le produzioni del non terminale T1
-- 8) ​T1​ ::= OPM F T1
-- Bisogna produrre
--  MULT LKC LKC 
--  DIV LKC LKC 
funt1 :: [Token] -> LKC -> Exc ([Token], LKC)
funt1 ((Symbol TIMES):b) po = do
                                (x, trad_f)  <- funf b
                                (y, trad_t1) <- funt1 x trad_f
                                Return (y, MULT po trad_t1)
funt1 ((Symbol DIVISION):b) po = do
                                   (x, trad_f)  <- funf b
                                   (y, trad_t1) <- funt1 x trad_f
                                   Return (y, DIV po trad_t1)
funt1 x po = Return (x, po) -- a causa di 8.2) T1 ::= Epsilon devo considerare anche il follow

-- ############################################################################
-- funf: funzione che rappresenta le produzioni del non terminale F
-- 9.2) ​F​ ::= exp_const
-- Produce: NUM Integer, BOO Bool, STRI String, NIL
funf :: [Token] -> Exc ([Token], LKC)
funf (a:b) = do
               (is_const, trad_exp_const) <- exp_const(a)
               if (is_const) then Return (b, trad_exp_const)  
                             else funf_contd (a:b)

-- funf_contd: continuato di funf.
-- 9.1) F ::= var Y
-- Bisogna produrre: 
--  VAR LKC se viene referenziato il nome di una variabile
--  CALL LKC [LKC] se dopo il token Id c'è una sequenza di espressioni
funf_contd :: [Token] -> Exc ([Token], LKC)
funf_contd ((Id a):b) = do
                          (x, trad_y) <- funy b
                          if (trad_y == [ETY]) then Return (x, VAR a)
                                               else Return (x, CALL (VAR a) trad_y)
-- 9.3) F ::= (ExpA)
-- Produce la compilazione di un'espressione algebrica
funf_contd ((Symbol LPAREN):b) = do
                                   (x, trad_expa) <- expa b
                                   y <- rec_rp x
                                   Return (y, trad_expa)
funf_contd (a:_)               = Raise ("ERRORE in funf_contd, TROVATO"++ show(a))

-- exp_const: funzione d'utilità che ritorna True se il token ricevuto come parametro
-- rappresenta un espressione costante.
exp_const::Token -> Exc (Bool, LKC)
exp_const (Number n) = Return (True, NUM n)
exp_const Nil        = Return (True, NIL)
exp_const (Bool b)   = Return (True, BOO b)
exp_const (String s) = Return (True, STRI s)
exp_const _          = Return (False, ETY)

-- ############################################################################
-- funy: funzione che rappresenta le produzioni del non terminale Y
-- 10) ​Y​ ::= (Seq_Exp)
funy :: [Token] -> Exc ([Token], [LKC])
funy ((Symbol LPAREN):b) =  do
                              (x, trad_seq_exp) <-seq_exp b
                              y <- rec_rp x
                              Return (y, trad_seq_exp)
funy x = Return (x, [ETY]) -- a causa di 10.2) Y ::= Epsilon devo considerare anche il follow

-- ############################################################################
-- seq_exp: funzione che rappresenta le produzioni del non terminale seq_exp
-- 14) ​Seq_Exp​ ::= Exp ; Seq_Exp
seq_exp:: [Token] -> Exc ([Token], [LKC])
seq_exp a@((Symbol RPAREN):b)   = Return (a, []) -- a causa di 14.2) Seq_Exp ::= Epsilon
                                                 -- devo considerare anche il follow
seq_exp a = do 
              (x, trad_exp) <- exp a
              (y, trad_exp_separator) <- exp_separator x
              Return (y, (trad_exp:trad_exp_separator))

-- ############################################################################
-- exp_separtor: unzione che rappresenta le produzioni del non terminale Exp_Separator
-- 15) Exp_Separator ::= , Exp Exp_Separtor
exp_separator:: [Token] -> Exc ([Token], [LKC])
exp_separator ((Symbol VIRGOLA):b) = do
                                    (z, trad_exp) <- exp b
                                    (t, trad_exp_separator) <- exp_separator z
                                    Return (t, (trad_exp:trad_exp_separator))
exp_separator a@((Symbol RPAREN):b) = Return (a, []) -- a causa di 15.2) Exp_Separator ::= Epsilon
                                                     -- devo considerare anche il follow
exp_separator (a:_) =  Raise ("ERRORE in exp_separator, TROVATO "++ show(a) ++" ATTESO )")

-- ############################################################################
-- seq_var: funzione che rappresenta le produzioni del non terminale seq_var
-- 16) Seq_Var​ ::= var Seq_Var
seq_var :: [Token] -> Exc ([Token], [LKC])
seq_var a@((Symbol RPAREN):b) = Return (a, []) -- a causa di 16.2) Seq_Var ::= Epsilon
                                               -- devo considerare anche il follow
seq_var ((Id a):b) = do
                       (z, trad_seq_var) <-seq_var b
                       Return (z, ((VAR a):trad_seq_var))
seq_var (a:_) =  Raise ("ERRORE in seq_var, TROVATO "++ show(a))
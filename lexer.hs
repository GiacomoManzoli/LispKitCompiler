module Lexer (
  Token(..),
  Symbol_T(..),
  Operator_T(..),
  Keyword_T(..),
  lexi -- Funzioni principale che esegue l'analisi lessicale lexi :: String -> [Token]
) where

import Prelude hiding (EQ)

-- ##############################################
--  Area per la definzione dei datatype
-- ##############################################

-- Datatype per le keyword del linguaggio
data Keyword_T = LET 
               | IN 
               | END 
               | LETREC 
               | AND 
               | IF 
               | THEN 
               | ELSE 
               | LAMBDA -- keyword per definire una funzione anonima
    deriving (Show,Eq)

-- Datatype per gli operatori del linguaggio
data Operator_T = EQ    -- uguaglianza
                | LEQ   -- <=
                | CAR   -- head di una lista
                | CDR   -- tail di una lista
                | CONS  -- concatenazione di due liste
                | ATOM  -- ??
    deriving (Show,Eq)

-- Datatype per i simboli del linguaggio
data Symbol_T = LPAREN -- (
              | RPAREN -- )
              | EQUALS 
              | PLUS 
              | MINUS 
              | TIMES 
              | DIVISION 
              | VIRGOLA   -- viene anche utilizzato per separare sequenze di espressioni
              | DOLLAR    -- simbolo che indica la terminazione del programma
    deriving (Show,Eq)

data Token = Keyword Keyword_T    -- token che rappresenta una keyword LispKit
           | Operator Operator_T  -- token che rappresenta un operatore, es: EQ, LEQ
           | Id String            -- token che rappresenta il nome di un identificatore
           | Symbol Symbol_T      -- token che rappresenta un simbolo, es: LPAREN, 
           | Number Integer       -- token che rappresenta un numero intero
           | String String        -- token che rappresenta una stringa
           | Bool Bool            -- token che rappresenta un booleano
           | Nil                  -- token che rappresenta un NIL
    deriving (Show,Eq)



-- Funzioni di supporto

-- Testa se il carattere è un carattere valido per iniziare un identificatore, un operatore o una keyword
isAlphaChar c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])

-- Riconosce se c è un carattere numerico o no
isDigitChar c = c `elem` ['0' .. '9']

-- Testa se il carattere è un carattere valido per comporre un identificatore, un operatore o una keyword (ad eccezione del primo carattere)
isIdChar c = isAlphaChar c || isDigitChar c

-- Testa se il carattere è un separatore
isSeparator c = c `elem` "()=$,"

-- Testa se è uno spazio o accapo
isSpace c = c `elem` [' ', '\n', '\f', '\r', '\t']

-- isSymbol c = c `elem` "()=+-*/,"
isSymbol c = c `elem` "()=+-*/,;" -- Aggiunto il ; per grammatica LL(1)


{- data una stringa X la confronta con le parole chiavi e con gli operatori
   del Lisp Kit e se è una di queste cose, restituisce la corrispondente
   coppia token_lexema, altrimenti la considera un identificatore e
   restituisce la coppia (ID, STRINGA(X)) -}
extractWord :: String -> Token
extractWord w = case w of
    "let"     -> Keyword LET
    "in"      -> Keyword IN
    "end"     -> Keyword END
    "letrec"  -> Keyword LETREC
    "and"     -> Keyword AND
    "if"      -> Keyword IF
    "then"    -> Keyword THEN
    "else"    -> Keyword ELSE
    "lambda"  -> Keyword LAMBDA
    
    "eq"      -> Operator EQ
    "leq"     -> Operator LEQ
    "car"     -> Operator CAR
    "cdr"     -> Operator CDR
    "cons"    -> Operator CONS
    "atom"    -> Operator ATOM
    
    "true"    -> Bool True
    "false"   -> Bool False
    
    "nil"     -> Nil
    
    otherwise -> Id w

toSymbol :: Char -> Symbol_T
toSymbol c = case c of
    '(' -> LPAREN
    ')' -> RPAREN
    '+' -> PLUS
    '-' -> MINUS
    '*' -> TIMES
    '/' -> DIVISION
    '=' -> EQUALS
    ',' -> VIRGOLA
    

{- Funzioni che implementano direttamente gli stati dell'automa. Osserva che
   non c'è ricorsione. Il passaggio dallo stato iniziale e principale I ad un
   altro stato è realizzato con un'invocazione. Poi si ritorna sempre a I e
   quindi basta il normale ritorno della funzione. -}

-- Stato N per riconoscere i numeri
{- n input numero segno
   n è la stringa in input
   numero è il numero elaborato finora
   segno è il segno del numero, true sse è negativo (rilevato da I) -}
n :: String -> Integer -> Bool -> (Token, String)
n "" _ _ = error "Unexpected end of string"
n input@(c:l) num sign
    | isDigitChar c =
        let d = read [c] :: Integer
        in n l (num*10 + d) sign
    | otherwise = (Number((if sign then -1 else 1) * num), input)

-- Stato SC per riconoscere le stringhe tra virgolette
{- sc input stringa
   stringa è la stringa elaborata finora 

*Lexer> let g3 = "\"Pluto\" Giacomo"
*Lexer> sc' g3
(String "","Pluto\" Giacomo")

   -}
sc :: String -> String -> (Token, String)
sc "" _ = error "Unexpected end of string"
sc ('"':l) res = (String res, l)
sc (c:l) res = sc l (res ++ [c])

-- Stato S per raccogliere le stringhe che possono corrispondere ad identificatori, operatori prefissi o keyword
{- 
   NB: Si occupa questa funzione di andare a mettere mano a creare il token corretto
   s input stringa
   stringa è l'identificatore elaborato finora 

   NB: perché questo funzioni la parole deve terminare con uno spazio.

   Il valore di ritorno (Token, String)
   - Token è il valore trovato
   - String è la l'input restante senza la stringa usata per creare il token
     Es: s "giacomo pluto" "" --> (Id "giacomo", " pluto")
-}
s :: String -> String -> (Token, String)
s "" _ = error "Unexpected end of string"
s input@(c:l) res
    | isIdChar c = s l (res ++ [c])
    | otherwise = (extractWord(res), input)



-- FUNZIONI DI SUPPORTO
{- 
  funzioni utili per nascondere i parametri ad hoc per le funzioni ricorsive
-}
n' :: String -> Bool -> (Token, String)
n' input sign = n input 0 sign

s' :: String -> (Token, String)
s' input = s input ""

sc' :: String -> (Token, String)
sc' input = sc input ""
-- FUNZIONE i DA FARE
{-
  Funzione ricorsiva che controlla i vari casi.
  - Stringa vuota -> errore
  - Stringa "$ -> fine
  - Stringa che inizia con uno spazio -> salta il carattere
-}
i :: String -> [Token]
i "" = error "Unexpected end of string"
i "$" = [(Symbol DOLLAR)]
{-
  f è un caratttere da analizzare e può essere:
  - un numero
  - il segno ~ (che conta per un meno)
  - un carattere alfabetico che inizia una variabile o una keyword

  l è il resto della stinga
-}
i input@(f:l)
    -- Riconoscimento dei numeri
    | f == '~' =
          if isDigitChar (head l) then     
            let (t,r) = n' l True -- scarto il carattere ~ e cerco un numero a partire dal carattere successivo
            in  t:(i r)
          else error "Hai cercato di mettere una stringa negativa."
    | isDigitChar f =                
          let (t,r) = n' input False 
          in  t:(i r)
    -- Riconoscimento dei simboli
    | isSymbol f =
          (Symbol $ toSymbol f):(i l)
    -- Trovo un carattere che può essere:
    -- - l'inizio del nome di una variabile
    -- - l'inizio di una keyword del linguaggio
    -- - l'inizio di una costante del linguaggio
    | isIdChar f = 
          let (t,r) = s' input
          in  t:(i r)
    -- Riconoscimento stringa constante
    | f == '\"' =
          -- Devo scartare f altrimenti sc' non trova la costante stringa
          let (t,r) = sc' l
          in  t:(i r)
    | isSpace f =
          i l
          
-- Funzione principale per l'analisi lessicale
lexi :: String -> [Token]
lexi = i



































#LispKit compiler

Progetto didattico per il corso di Linguaggi di Programmazione della Laurea Magistrale in Informatica presso l'Università degli Studi di Padova (AA 2015-2016).

Il progretto consiste nella realizzazione di un compilatore per il linguaggio LispKit, le specifiche sono disponibili nel file `lexi-document.pdf`.

## Parti del progetto

### Parte 1 - Analizzatore lessicare

La prima parte prevedere la realizzazione di un analizzatore lessicale che risca a trasformare una stringa in una lista di token.

L'analizzatore è definito nel file `lexer.hs`

### Parte 2 - Grammatica LL(1)

Per come è definita la grammatica del linguaggio LispKit, questa non è LL(1), pertanto è necessario andare a modificarla.

La nuova versione della grammatica è descritta in `grammatica-ll1.pdf`

### Parte 3 - Analisi sintattica

Per verificare la correttezza della sequenza di token è necessario  implementare un analizzatore sintattico che sia in grado di stabilire se la sequenza di token rispetta o meno le produzioni della grammatica del linguaggio.

La prima versione dell'analizzatore sintattico si trova nel file `analizzatore_parte3.hs`.

### Parte 4 - Analisi sintattica 2.0

L'analizzatore della parte 3 verifica solamente la sequenza di token senza costruire l'albero astratto che rappresenta il programma.

Dal momento che tale albero è utile per compilare il codice, l'analizzatore è stato modificato in modo che costruisca l'albero mentre esamina la sequenza di token.

Il nuovo analizzatore si trova nel file `analizzatore.hs`

### Parte 5 - Compilazione

Sfruttando l'albero prodotto si riescono a convertire le istruzioni `LKC` in `Secdexpr` in modo che la macchina SECD riesca ad interpretarle.

Il file `compilatore.hs` contiene la definzione del compilatore, mentre `interprete.hs` contiene una macchina SECD in grado di intepretare le istruzioni `Secdexpr`.

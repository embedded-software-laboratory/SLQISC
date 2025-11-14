# SubleqCompiler

Basic Cabal project. Can be built with `cabal build` and ran with `cabal run assembler -- <filename.asm>`. An optional `-v` parameter allows for generation of debug logs, a CFA and intermediate compilation steps.

## Befehle

- „SLQ a b c“ ist das einfache SUBLEQ a b c, nur in weniger zu tippen
- „NOP“ tut nichts
- „MOV a b“ kopiert den Wert von Adresse b nach Adresse a
- „STI a b“ kopiert den Wert von Adresse b an die Adresse, die an Adresse a steht
- „LDI a b“ kopiert den Wert von der Adresse, die an Adresse b steht, nach Adresse a
- „PUSH a“ kopiert den Wert von Adresse a an das Ziel des Stackpointers (Adresse -3) und Dekrementiert den Stackpointer
- „POP a“ inkrementiert den Stackpointer und kopiert den Wert vom Ziel des Stackpointers nach Adresse a
- „ADD a b“ addiert den Wert an Adresse b auf den Wert an Adresse a
- „SUB a b“ subtrahiert den Wert an Adresse b vom Wert an Adresse a
- „MUL a b“ multipliziert den Wert an Adresse b auf den Wert an Adresse a
- „DIV a b“ dividiert den Wert an Adresse a durch den Wert an Adresse b
- „MOD a b“ setzt den Wert an Adresse a auf a modulo b
- „NEG a“ negiert den Wert an Adresse a (Zweierkomplement)
- „AND a b“ verundet den Wert an Adresse a mit dem Wert an Adresse b (logisches und, Ergebnis ist 1 wenn a=b=1, sonst 0, Eingaben sollten nur 0 und 1 sein)
- „OR a b“ verodert den Wert an Adresse a mit dem Wert an Adresse b
- „NOT a“ negiert den Wert an Adresse a (Boolesch)
- „JMP a“ springt an Adresse a
- “JLEQ a b” springt an Adresse b falls der Wert an Adresse a <= 0 ist
- „INC a“ inkrementiert den Wert an Adresse a
- „DEC a“ dekrementiert den Wert an Adresse a
- „IN a“ blockiert die Ausführung des Programms bis eine Eingabe getätigt wird und speichert die Eingabe an Adresse a
- „OUT a“ gibt den Wert an Adresse a aus
- „PRNT a“ gibt den Wert an Adresse a als Dezimalzahl aus
- „DOUT a“ gibt den Wert an Adresse a als Dezimalziffer aus
- „STR a“ speichert den String a charweise als Wert an mehreren hintereinanderliegenden Adressen
- „CALL a“ springt an Adresse a und speichert die Rücksprungadresse auf dem Stack (effektiv: PUSH <nextLoc>, JMP a)
- „RET“ liest den obersten Wert vom Stack und springt an diese Adresse (effektiv, POP x, JMP x)

Der Assembler ist Case-Sensitive – Befehle sind immer in CAPS und Labels sind immer klein.

## Argumente

Argumente für die Befehle sind Zahlen (für Adressen) oder Labels. Es gibt einige Spezialwerte:

- #c ist der eine Adresse, hinter der Konstante c liegt. Beispiel:  MOV 17 #42 speichert den Wert 42 an Adresse 17.
- @c ist eine Adresse, hinter der eine Konstante liegt, die den char ‚c‘ codiert
- $i ist eine reservierte Speicherstelle, die einem Register bei normalen CPUs nahekommt, für 0 <= i <16
- $I ist die Speicherstelle an der Inputs gelesen werden können
- $O ist die Speicherstelle an die Outputs geschrieben werden können
- $SP ist der Stackpointer

## Sektionen

Assemblycode ist in Sektionen unterteilt, „SECTION name“ leitet eine Sektion ein, die vom Assembler platziert wird, „SECTION name[@address]“ platziert die Sektion an der angegebenen Adresse. Da die Ausführung immer an Adresse 0 startet, sollte es eine Sektion geben, die explizit an Adresse 0 steht, um den Programmstart korrekt zu setzen.

Beispielassemblyprogramme finden Sie im Ordner Assembler/programs.

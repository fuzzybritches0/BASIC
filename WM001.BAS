DECLARE SUB Synchron ()
DECLARE SUB Split (L$)
DECLARE SUB New ()
DECLARE SUB Learn ()
DECLARE SUB SYS ()
DECLARE SUB FileLoad ()
DECLARE SUB FileSave ()
DECLARE SUB Delete ()
DECLARE SUB Find ()
DECLARE SUB EnterWords ()
DECLARE SUB MORVE (VERS$)
DECLARE SUB REMORVE (VERS$)
DECLARE SUB Weekday (Day$)
DECLARE SUB Login ()
DECLARE SUB SetPalette ()
DECLARE SUB MakeWindow (X!, Y!, X2!, Y2!, FG!, BG!)
DECLARE SUB Eingabe (Zeile!, Spalte!, MAXLAENGE!, IN$)
DIM SHARED VocReal$(5), Voc$(5), FileNew, Word$, Forign1$, Forign2$, Forign3$
DIM SHARED Eror, file$
DIM SHARED HowMany(6)
ON ERROR GOTO FEHLER
SetPalette
CLS
LOCATE 25, 1: PRINT "[WM]WordMinder BETA 0.01 (C) by cm-SoFT, all rights reserved, 2001"
Base1:
MakeWindow 1, 1, 21, 80, 4, 6
LOCATE 4, 2: PRINT STRING$(78, 205)
LOCATE 23, 1: PRINT "command:"
COLOR 7
LOCATE 2, 2: PRINT "okay"
IF FileNew = 0 THEN LOCATE 3, 2: PRINT "Load or create new file!"
COLOR 14
LOCATE 5, 2: PRINT "commands:"
LOCATE 6, 2: PRINT "---------"
LOCATE 8, 2: PRINT "new    - Create new files (Databasefile + LearnEngine)"
LOCATE 9, 2: PRINT "load   - Load existing WordDatabase and LearnEngine"
LOCATE 10, 2: PRINT "save   - Save WordDatabase and LearnEngine"
LOCATE 11, 2: PRINT "delete - Delete word from Database or LearnEngine"
LOCATE 12, 2: PRINT "find   - Find word in Database or LearnEngine"
LOCATE 13, 2: PRINT "enter  - Enter words into Database"
LOCATE 14, 2: PRINT "learn  - Start LearnEngine"
LOCATE 15, 2: PRINT "quit   - Exit to OS"
IF NOT ERR = 0 THEN COLOR 2: GOTO FEHLERBE
IF NOT Eror = 0 THEN COLOR 2: GOTO Fehler2
comm$ = ""
COLOR 7
Eingabe 23, 9, 72, comm$
COLOR 3
IF comm$ = "quit" THEN SYS: GOTO Base1
IF comm$ = "enter" THEN EnterWords: GOTO Base1
IF comm$ = "delete" THEN Delete: GOTO Base1
IF comm$ = "find" THEN Find: GOTO Base1
IF comm$ = "load" THEN FileLoad: GOTO Base1
IF comm$ = "save" THEN FileSave: GOTO Base1
IF comm$ = "new" THEN New: GOTO Base1
IF comm$ = "learn" THEN Learn: GOTO Base1
IF comm$ = "" THEN GOTO Base1
LOCATE 3, 2: PRINT "command not found!                  ": SLEEP 2: GOTO Base1

FEHLER:
GOTO Base1

FEHLERBE:
BEEP
COLOR 2
LOCATE 2, 2: PRINT "ERROR: "; ERR
LOCATE 3, 2: PRINT STRING$(70, " ")
LOCATE 3, 2: PRINT "Error unknown!"
IF ERR = 53 THEN LOCATE 3, 2: PRINT "file not found!"
IF ERR = 64 THEN LOCATE 3, 2: PRINT "unexepted filename!"
SLEEP 2
COLOR 4
RESUME Base1

Fehler2:
BEEP
COLOR 2
LOCATE 2, 2: PRINT "ERROR: "; Eror + 100
LOCATE 3, 2: PRINT STRING$(70, " ")
IF Eror = 1 THEN LOCATE 3, 2: PRINT "No File Loaded!"
IF Eror = 2 THEN LOCATE 3, 2: PRINT "First quit program!"
SLEEP 2
COLOR 4
Eror = 0
GOTO Base1

SUB Delete
IF FileNew = 0 THEN Eror = 1: GOTO EndeDelete
AgainDelete:
MakeWindow 1, 1, 21, 80, 4, 6
word2$ = ""
COLOR 6, 4
LOCATE 1, 34: PRINT "Delete word"
COLOR 2, 6
LOCATE 3, 5: PRINT "find:"
LOCATE 4, 2: PRINT STRING$(78, 205)
Eingabe 3, 11, 60, word2$
IF word2$ = "" THEN GOTO EndeDelete

FOR SearchAll = 1 TO 6
IF SearchAll = 1 THEN SearchF$ = "wm.tmp": FoundIn$ = "Database"
IF SearchAll = 2 THEN SearchF$ = Voc$(1): FoundIn$ = "Shaft 1"
IF SearchAll = 3 THEN SearchF$ = Voc$(2): FoundIn$ = "Shaft 2"
IF SearchAll = 4 THEN SearchF$ = Voc$(3): FoundIn$ = "Shaft 3"
IF SearchAll = 5 THEN SearchF$ = Voc$(4): FoundIn$ = "Shaft 4"
IF SearchAll = 6 THEN SearchF$ = Voc$(5): FoundIn$ = "Shaft 5"
 OPEN SearchF$ FOR INPUT AS #1
 OPEN "tmp.tmp" FOR OUTPUT AS #2
 PRINT #2, "cm-SoFT [WMV0.01]"
 LINE INPUT #1, nul$
 DO WHILE NOT EOF(1)
  LINE INPUT #1, finde$
  FOR findword = 1 TO LEN(finde$)
   IF MID$(finde$, findword, LEN(word2$)) = LCASE$(word2$) THEN
     GOSUB FindNow2
     GOTO fromhere22
   END IF
  NEXT findword
  PRINT #2, finde$
fromhere22:
 LOOP

 CLOSE #1
 CLOSE #2
 KILL SearchF$
 RENSF$ = "ren tmp.tmp " + SearchF$
 SHELL RENSF$
NEXT SearchAll

 IF Foundd = 0 THEN LOCATE 12, 3: PRINT "word not found!": SLEEP 2
 Foundd = 0
GOTO AgainDelete



FindNow2:
Foundd = Foundd + 1
Split finde$
LOCATE 8, 3: PRINT "     found in: "; FoundIn$; "      "
LOCATE 9, 3: PRINT "-------------------------------"
LOCATE 12, 3: PRINT "        word: "; Word$
LOCATE 13, 3: PRINT "translation1: "; Forign1$
LOCATE 14, 3: PRINT "translation2: "; Forign2$
LOCATE 15, 3: PRINT "translation3: "; Forign3$
yn$ = "n"
LOCATE 18, 3: PRINT "Delete y/n"
Eingabe 19, 3, 3, yn$
IF yn$ = "y" THEN RETURN
PRINT #2, finde$
RETURN

EndeDelete:
END SUB

SUB Eingabe (Zeile, Spalte, MAXLAENGE, IN$)
100 REM *****************************************
110 REM *** komfortable Eingaberoutine  sp140688
120 REM *****************************************
130 :
140 GOSUB 61000                                      ' Initialisierung
150 :
190 ZULASS$ = BUCHSTABEN$ + ZAHLEN$ + SONDER$        ' erlaubte Zeichen
191 IF MAXLAENGE > 80 THEN MAXLAENGE = MAXLAENGE - 80: ZULASS$ = ZAHLEN$
210 :
230 GOSUB 60000                                      ' Eingaberoutine
240 :
270 :
280 GOTO EndeEingabe
290 :
60000 :
60010 REM ************************************
60020 REM ********** Eingaberoutine **********
60030 REM ************************************
60040 :
60050 REM *** Initialisierung ***
60060 :
60070 Anfang = LEN(IN$) + 1                        ' Cursor-Anfangsposition
60080 EINFUEGFLAG = 0                              ' öberschreibmodus ein
60090 LOCATE Zeile, Spalte                         ' Eingabe positionieren
60100 IN$ = IN$ + STRING$(MAXLAENGE - LEN(IN$), " ") ' Vorgabe erstellen
60110 PRINT IN$                                      ' Vorgabe ausgeben
60120 :
60130 REM *** Stringeingabe ***
60140 :
60150 FLAG = 0
60160 WHILE FLAG = 0 OR INSTR(ABSCHLUSS$, A$) = 0
60170 :  LOCATE Zeile, Spalte + Anfang - 1, 1
60180 :  FLAG = 0
60190 :  A$ = INKEY$
60200 :  IF A$ = "" THEN 60190
60210 :  IF ASC(LEFT$(A$, 1)) < 32 THEN FLAG = 1: A$ = RIGHT$(A$, 1)
60220 :  LOCATE , , 0
60230 :  IF INSTR(ZULASS$, A$) <> 0 AND FLAG = 0 THEN GOSUB 60370
60240 :  IF INSTR(STEUER$, A$) <> 0 AND FLAG THEN GOSUB 60570
60250 WEND
60260 :
60270 REM *** Leerzeichen am Stringende entfernen ***
60280 :
60290 WHILE RIGHT$(IN$, 1) = " " AND LEN(IN$) <> 0
60300 :  IN$ = LEFT$(IN$, LEN(IN$) - 1)
60310 WEND
60320 LOCATE , , 0                              ' Cursor ausschalten
60330 RETURN
60340 :
60350 REM *** Zeicheneingabe ***
60360 :
60370 IF EINFUEGFLAG THEN 60490                 ' öberschreiben ein/aus ?
60380 :
60390 REM * öberschreibmodus *
60400 :
60410 MID$(IN$, Anfang, 1) = A$                 ' Zeichen durch Eingabe ersetzen
60420 LOCATE Zeile, Spalte + Anfang - 1
60430 PRINT A$                       ' Zeichen ausgeben
60440 IF Anfang < MAXLAENGE THEN Anfang = Anfang + 1    ' Zeichenposition aktualisieren
60450 RETURN
60460 :
60470 REM * Einfuegemodus *
60480 :
60490 IF RIGHT$(IN$, 1) <> " " THEN RETURN      ' Noch Platz im String ?
60500 IN$ = LEFT$(IN$, Anfang - 1) + A$ + MID$(IN$, Anfang, LEN(IN$) - Anfang)  'Zeich. einfÅgen
60510 LOCATE Zeile, Spalte
60520 PRINT IN$                                 ' Zeichen ausgeben
60530 GOTO 60440
60540 :
60550 REM *** Steuertasten ***
60560 :
60570 ON INSTR(STEUER$, A$) GOSUB 60620, 60670, 60700, 60750, 60800, 60870, 60960, EndeEingabe, EndeESC
60580 RETURN
60590 :
60600 REM * Cursor-Right-Taste *
60610 :
60620 IF Anfang < MAXLAENGE THEN Anfang = Anfang + 1' Cursor um 1 Zeichen n. rechts
60630 RETURN
60640 :
60650 REM * Cursor-Left-Taste *
60660 :
60670 IF Anfang > 1 THEN Anfang = Anfang - 1     ' Cursor um 1 Zeichen n. links
60680 RETURN
60690 :
60700 REM * Home-Taste *
60710 :
60720 Anfang = 1                                ' Cursor an Anfang der Eingabe
60730 RETURN
60740 :
60750 REM * End-Taste *
60760 :
60770 Anfang = MAXLAENGE                        ' Cursor an Ende der Eingabe
60780 RETURN
60790 :
60800 REM * Delete-Taste *
60810 :
60820 IN$ = LEFT$(IN$, Anfang - 1) + RIGHT$(IN$, MAXLAENGE - Anfang) + " "                                                      ' String korrigieren
60830 LOCATE Zeile, Spalte
60840 PRINT IN$                                 ' String ausgeben
60850 RETURN
60860 :
60870 REM * Backspace-Taste *
60880 :
60890 IF Anfang = 1 THEN 60940                  ' Schon am Stringanfang ?
60900 IN$ = LEFT$(IN$, Anfang - 2) + RIGHT$(IN$, MAXLAENGE - Anfang + 1) + " "                                                  ' String korrigieren
60910 LOCATE Zeile, Spalte
60920 PRINT IN$                                 ' Ausgabe des Eingabestrings
60930 Anfang = Anfang - 1                       ' Zeichenposition aktualisieren
60940 RETURN
60950 :
60960 REM * Ins-Taste *
60970 :
60980 EINFUEGFLAG = EINFUEGFLAG XOR 1           ' EinfÅgeflag umschalten
60990 RETURN
61000 :
61010 REM *** Initialisierung ***
61020 :
61030 STEUER$ = CHR$(77) + CHR$(75) + CHR$(71) + CHR$(79) + CHR$(83) + CHR$(8) + CHR$(82) + CHR$(13) + CHR$(27)                           ' Steuerzeichen
61040 ABSCHLUSS$ = CHR$(59)                     ' RETURN = Abschlu· der Eingabe
61050 ZAHLEN$ = "1234567890+-."                 ' Zeichen fÅr Zahleneingaben
61060 BUCHSTABEN$ = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRTSUVW ÑîÅéôö·"                                                 ' Alphanumerische Zeichen
61070 SONDER$ = " !4$%&/()=?`'[]*#^,.-:;_><\+-"                                                                                 ' Sonderzeichen
61080 RETURN

EndeESC:
IN$ = "ESC"
GOTO FINALEND

EndeEingabe:
GOSUB 60270

FINALEND:
END SUB

SUB EnterWords
IF FileNew = 0 THEN Eror = 1: GOTO EndeWords
OPEN "wm.tmp" FOR APPEND AS #1
AgainWord:
MakeWindow 1, 1, 21, 80, 4, 6
Word$ = ""
Forign1$ = ""
Forign2$ = ""
Forign3$ = ""

COLOR 6, 4
LOCATE 1, 35: PRINT "Enter word"
COLOR 8, 6
LOCATE 5, 3: PRINT "        word:"
LOCATE 7, 3: PRINT "translation1:"
LOCATE 8, 3: PRINT "translation2:"
LOCATE 9, 3: PRINT "translation3:"
LOCATE 4, 2: PRINT STRING$(78, 205)
LOCATE 11, 2: PRINT STRING$(78, 205)
LOCATE 19, 2: PRINT STRING$(78, 205)
LOCATE 13, 3: PRINT "  Last Input:"
LOCATE 14, 3: PRINT "-------------"
LOCATE 15, 3: PRINT "        word: "; liw$
LOCATE 16, 3: PRINT "translation1: "; lif1$
LOCATE 17, 3: PRINT "translation2: "; lif2$
LOCATE 18, 3: PRINT "translation3: "; lif3$
Eingabe 5, 17, 60, Word$
 IF Word$ = "" THEN GOTO EndeWords
Entered = 1
Eingabe 7, 17, 60, Forign1$
 IF Forign1$ = "" THEN GOTO EndeWords
Eingabe 8, 17, 60, Forign2$
Eingabe 9, 17, 60, Forign3$

line$ = Word$ + CHR$(179) + Forign1$
IF NOT Forign2$ = "" THEN line$ = line$ + CHR$(180) + Forign2$
IF NOT Forign3$ = "" THEN line$ = line$ + CHR$(180) + Forign3$
PRINT #1, LCASE$(line$)
liw$ = LCASE$(Word$)
lif1$ = LCASE$(Forign1$)
lif2$ = LCASE$(Forign2$)
lif3$ = LCASE$(Forign3$)
GOTO AgainWord:

EndeWords:
CLOSE #1
END SUB

SUB FileLoad
MakeWindow 1, 1, 21, 80, 4, 6
IF FileNew = 1 THEN GOTO firstsave
LoadAnyWay:
COLOR 6, 4
LOCATE 1, 34: PRINT "Load File"
COLOR 1, 6
LOCATE 6, 3: PRINT "file:        .dat"
Eingabe 6, 8, 8, file$
IF file$ = "" GOTO ThatLoad
filee$ = file$ + ".dat"
Voc$(1) = file$ + ".s1"
Voc$(2) = file$ + ".s2"
Voc$(3) = file$ + ".s3"
Voc$(4) = file$ + ".s4"
Voc$(5) = file$ + ".s5"
VocReal$(1) = file$ + ".sc1"
VocReal$(2) = file$ + ".sc2"
VocReal$(3) = file$ + ".sc3"
VocReal$(4) = file$ + ".sc4"
VocReal$(5) = file$ + ".sc5"
FOR CopyReal = 1 TO 5
Core$ = "copy " + VocReal$(CopyReal) + " " + Voc$(CopyReal) + " /Y >nul"
SHELL Core$
NEXT CopyReal
OPEN filee$ FOR INPUT AS #1
CLOSE #1

CO$ = "copy " + file$ + ".dat" + " wm.tmp /Y >nul"
SHELL CO$

FileNew = 1
GOTO ThatLoad
firstsave:
LOCATE 15, 3: PRINT "file already loaded!"
LOCATE 16, 3: PRINT "load new file anyway?"
yn$ = "n"
Eingabe 17, 3, 3, yn$
IF yn$ = "n" GOTO ThatLoad ELSE GOTO LoadAnyWay

ThatLoad:
END SUB

SUB FileSave
MakeWindow 1, 1, 21, 80, 4, 6
IF FileNew = 0 THEN Eror = 1: GOTO Endend
COLOR 6, 4
LOCATE 1, 34: PRINT "Save File"
COLOR 11, 6
LOCATE 6, 3: PRINT "file:        .dat"
Eingabe 6, 8, 8, file$
CO$ = "copy wm.tmp " + file$ + ".dat /Y >nul"
SHELL CO$
VocReal$(1) = file$ + ".sc1"
VocReal$(2) = file$ + ".sc2"
VocReal$(3) = file$ + ".sc3"
VocReal$(4) = file$ + ".sc4"
VocReal$(5) = file$ + ".sc5"
FOR CopyReal = 1 TO 5
Core$ = "copy " + Voc$(CopyReal) + " " + VocReal$(CopyReal) + " /Y >nul"
SHELL Core$
NEXT CopyReal

Endend:

END SUB

SUB Find
IF FileNew = 0 THEN Eror = 1: GOTO EndeFind
AgainFind:
MakeWindow 1, 1, 21, 80, 4, 6
word2$ = ""
COLOR 6, 4
LOCATE 1, 35: PRINT "Find word"
COLOR 7, 6
LOCATE 3, 5: PRINT "find:"
LOCATE 4, 2: PRINT STRING$(78, 205)
Eingabe 3, 11, 60, word2$
IF word2$ = "" THEN GOTO EndeFind

FOR SearchAll = 1 TO 6
IF SearchAll = 1 THEN SearchF$ = "wm.tmp": FoundIn$ = "Database"
IF SearchAll = 2 THEN SearchF$ = Voc$(1): FoundIn$ = "Shaft 1"
IF SearchAll = 3 THEN SearchF$ = Voc$(2): FoundIn$ = "Shaft 2"
IF SearchAll = 4 THEN SearchF$ = Voc$(3): FoundIn$ = "Shaft 3"
IF SearchAll = 5 THEN SearchF$ = Voc$(4): FoundIn$ = "Shaft 4"
IF SearchAll = 6 THEN SearchF$ = Voc$(5): FoundIn$ = "Shaft 5"

OPEN SearchF$ FOR INPUT AS #1
LINE INPUT #1, nul$
 DO WHILE NOT EOF(1)
  LINE INPUT #1, finde$
  FOR findword = 1 TO LEN(finde$)
   IF MID$(finde$, findword, LEN(word2$)) = LCASE$(word2$) THEN
    GOSUB FindNow
    GOTO fromHere2
   END IF
  NEXT findword
fromHere2:
 LOOP
 CLOSE #1
NEXT SearchAll

IF Foundd = 0 THEN LOCATE 12, 3: PRINT "word not found!": SLEEP 2
Foundd = 0
GOTO AgainFind

FindNow:
Foundd = Foundd + 1
Split finde$
LOCATE 8, 3: PRINT "     found in: "; FoundIn$; "      "
LOCATE 9, 3: PRINT "-------------------------------"
LOCATE 12, 3: PRINT "        word: "; Word$
LOCATE 13, 3: PRINT "translation1: "; Forign1$
LOCATE 14, 3: PRINT "translation2: "; Forign2$
LOCATE 15, 3: PRINT "translation3: "; Forign3$
Eingabe 17, 2, 3, yn$
RETURN

EndeFind:
END SUB

SUB Learn
MakeWindow 1, 1, 21, 80, 4, 6
COLOR 6, 4
LOCATE 1, 34: PRINT "LearnEngine"
COLOR 4, 6
IF FileNew = 0 THEN Eror = 1: GOTO EndeLearn2
Schacht1 = 80
Schacht2 = 130
Schacht3 = 200
Schacht4 = 350
Schacht5 = 10000
EnterHow:
HowMany(1) = 0
OPEN "wm.tmp" FOR INPUT AS #1
LINE INPUT #1, nul$
DO WHILE NOT EOF(1)
LINE INPUT #1, null$
HowMany(1) = HowMany(1) + 1
LOOP
CLOSE #1
HowMany(2) = 0
OPEN Voc$(1) FOR INPUT AS #1
LINE INPUT #1, nul$
DO WHILE NOT EOF(1)
LINE INPUT #1, nul$
HowMany(2) = HowMany(2) + 1
LOOP
CLOSE #1

IF NOT HowMany(2) = 0 THEN GOTO LearnNow
IF HowMany(1) = 0 THEN GOTO EndeLearn
LOCATE 2, 3: PRINT "How many words move into LearnEngine?"
MAX = Schacht1 - HowMany(2)
IF HowMany(1) < MAX THEN MAX = HowMany(1)
LOCATE 3, 3: PRINT "max.:               "
LOCATE 4, 2: PRINT STRING$(78, 205)
nul1$ = STR$(MAX)
Eingabe 3, 9, 10, nul1$
Account = VAL(nul1$)
IF Account = 0 THEN GOTO EndeLearn
IF Account > HowMany(1) GOTO EnterHow
IF Account > Schacht1 - HowMany(2) THEN GOTO EnterHow

MoveWord = 0
OPEN "wm.tmp" FOR INPUT AS #1
OPEN Voc$(1) FOR APPEND AS #2
LINE INPUT #1, nul$
DO WHILE NOT EOF(1)
LINE INPUT #1, nul$
MoveWord = MoveWord + 1
PRINT #2, nul$
IF MoveWord = Account THEN
  CLOSE #2
  OPEN "wm2.tmp" FOR OUTPUT AS #2
PRINT #2, "cm-SoFT [WMV0.01]"
END IF
LOOP
CLOSE #1
CLOSE #2
KILL "wm.tmp"
SHELL "ren wm2.tmp wm.tmp"

LearnNow:
FOR ClearHM = 2 TO 6
HowMany(ClearHM) = 0
NEXT ClearHM
FOR DoAlso = 1 TO 5
OPEN Voc$(DoAlso) FOR INPUT AS #1
LINE INPUT #1, nul$
DO WHILE NOT EOF(1)
LINE INPUT #1, null$
HowMany(DoAlso + 1) = HowMany(DoAlso + 1) + 1
LOOP
CLOSE #1
NEXT DoAlso

IF HowMany(2) = 0 THEN GOTO EnterHow
CurrentFile$ = Voc$(1): CurrentFile2$ = Voc$(2)
IF HowMany(6) >= Schacht5 THEN CurrentFile$ = Voc$(5): CurrentFile2$ = "wm.tmp"
IF HowMany(5) >= Schacht4 THEN CurrentFile$ = Voc$(4): CurrentFile2$ = Voc$(5)
IF HowMany(4) >= Schacht3 THEN CurrentFile$ = Voc$(3): CurrentFile2$ = Voc$(4)
IF HowMany(3) >= Schacht2 THEN CurrentFile$ = Voc$(2): CurrentFile2$ = Voc$(3)
IF HowMany(2) >= Schacht1 THEN CurrentFile$ = Voc$(1): CurrentFile2$ = Voc$(2)

OPEN CurrentFile$ FOR INPUT AS #1
LINE INPUT #1, nul$
LINE INPUT #1, nul$
CLOSE #1
Split nul$
LOCATE 6, 3: PRINT STRING$(78, 205)
LOCATE 17, 3: PRINT STRING$(78, 205)
FOR WPL = 1 TO 9
LOCATE 7 + WPL, 3: PRINT STRING$(65, " ")
NEXT WPL
LOCATE 7, 3: PRINT "  translation: "
LOCATE 8, 3: PRINT "         word: "; Word$
LOCATE 9, 2: PRINT STRING$(78, 205)
LOCATE 11, 3: PRINT "  Last check:"
LOCATE 12, 3: PRINT "        word: "; WordO$
LOCATE 13, 3: PRINT "translation1: "; Forign1O$
LOCATE 14, 3: PRINT "translation2: "; Forign2O$
LOCATE 15, 3: PRINT "translation3: "; Forign3O$
LOCATE 16, 3: PRINT "  Last check: "; SETRF$
Forign1O$ = Forign1$
Forign2O$ = Forign2$
Forign3O$ = Forign3$
WordO$ = Word$
CheckIf$ = ""
Eingabe 7, 18, 60, CheckIf$
IF CheckIf$ = "" THEN GOTO EndeLearn
CheckIf$ = LCASE$(CheckIf$)
IF CheckIf$ = Forign1$ OR CheckIf$ = Forign2$ OR CheckIf$ = Forign3$ THEN
  SETRF$ = "Move into next shaft"
  OPEN CurrentFile2$ FOR APPEND AS #1
  PRINT #1, nul$
  CLOSE #1
  OPEN CurrentFile$ FOR INPUT AS #1
  LINE INPUT #1, nul$
  LINE INPUT #1, nul$
  OPEN "wm22.tmp" FOR OUTPUT AS #2
  PRINT #2, "cm-SoFT [WMV0.01]"
  DO WHILE NOT EOF(1)
   LINE INPUT #1, nul$
   PRINT #2, nul$
  LOOP
  CLOSE #1
  CLOSE #2
  REEN$ = "ren wm22.tmp " + CurrentFile$
  REEN2$ = "del " + CurrentFile$ + " >nul"
  SHELL REEN2$
  SHELL REEN$
  GOTO LearnNow
  END IF
SETRF$ = "Move back to shaft 1"
OPEN Voc$(1) FOR APPEND AS #1
PRINT #1, nul$
CLOSE #1
  OPEN CurrentFile$ FOR INPUT AS #1
  LINE INPUT #1, nul$
  LINE INPUT #1, nul$
  OPEN "wm22.tmp" FOR OUTPUT AS #2
  PRINT #2, "cm-SoFT [WMV0.01]"
  DO WHILE NOT EOF(1)
  LINE INPUT #1, nul$
  PRINT #2, nul$
  LOOP
  CLOSE #1
  CLOSE #2
  REEN$ = "ren wm22.tmp " + CurrentFile$
  REEN2$ = "del " + CurrentFile$ + " >nul"
  SHELL REEN2$
  SHELL REEN$
GOTO LearnNow

EndeLearn:
EndeLearn2:
END SUB

SUB MakeWindow (X, Y, X2, Y2, FG, BG)
X2 = X2 + 1
Y2 = Y2 - 2
COLOR FG, BG
LOCATE X, Y: PRINT "⁄"; STRING$(Y2, "ƒ"); "ø"
FOR RestBody = 1 TO X2 - 2
LOCATE X + RestBody, Y: PRINT "¿"; STRING$(Y2, "ƒ"); "Ÿ"
LOCATE X + RestBody, Y: PRINT "≥"; STRING$(Y2, " "); "≥"
NEXT RestBody
LOCATE X2, Y: PRINT "¿"; STRING$(Y2, "ƒ"); "Ÿ"
END SUB

SUB MORVE (VERS$)    '80Byte VerschlÅsselungsroutine
ALT$ = VERS$
VERS$ = ""
FOR morfe = 1 TO LEN(ALT$)
IF morfe = 81 THEN GOTO BREAKEMORFE
VERS$ = VERS$ + CHR$(ASC(MID$(ALT$, morfe, 1)) + 10 + morfe)
NEXT morfe
BREAKEMORFE:
END SUB

SUB New
CreateNewFile:
IF FileNew = 1 THEN Eror = 2: GOTO EndeNew
MakeWindow 1, 1, 21, 80, 4, 6
LOCATE 3, 3: PRINT "create file"
LOCATE 4, 3: PRINT "-----------"
LOCATE 6, 3: PRINT "file:        .dat"
Eingabe 6, 8, 8, file$
IF file$ = "" GOTO EndeNew
filee$ = file$ + ".dat"
OPEN filee$ FOR OUTPUT AS #1
PRINT #1, "cm-SoFT [WMV0.01]"
CLOSE #1
Voc$(1) = file$ + ".s1"
Voc$(2) = file$ + ".s2"
Voc$(3) = file$ + ".s3"
Voc$(4) = file$ + ".s4"
Voc$(5) = file$ + ".s5"
FOR createIt = 1 TO 5
 OPEN Voc$(createIt) FOR OUTPUT AS #1
 PRINT #1, "cm-SoFT [WMV0.01]"
 CLOSE #1
NEXT createIt
FOR createIt = 1 TO 5
 OPEN VocReal$(createIt) FOR OUTPUT AS #1
 PRINT #1, "cm-SoFT [WMV0.01]"
 CLOSE #1
NEXT createIt
FileNew = 1
OPEN "wm.tmp" FOR OUTPUT AS #1
PRINT #1, "cm-SoFT [WMV0.01]"
CLOSE #1
Entered = 1
EndeNew:
END SUB

SUB REMORVE (VERS$)
ALT$ = VERS$
VERS$ = ""
FOR morfe = 1 TO LEN(ALT$)
IF morfe = 81 THEN GOTO BREAKEMORFE2
VERS$ = VERS$ + CHR$(ASC(MID$(ALT$, morfe, 1)) - 10 - morfe)
NEXT morfe
BREAKEMORFE2:
END SUB

SUB SetPalette
PALETTE 0, 0
PALETTE 1, 1
PALETTE 2, 20
PALETTE 3, 21
PALETTE 4, 23
PALETTE 5, 24
PALETTE 6, 25
PALETTE 7, 26
PALETTE 8, 27
PALETTE 9, 28
PALETTE 10, 29
PALETTE 11, 30
PALETTE 12, 31
PALETTE 13, 32
PALETTE 14, 33
PALETTE 15, 34
END SUB

SUB Split (L$)
Word$ = ""
Forign1$ = ""
Forign2$ = ""
Forign3$ = ""

FOR SplitIt = 1 TO LEN(L$)
IF MID$(L$, SplitIt, 1) = CHR$(179) THEN Location = 1: GOTO Jump
IF MID$(L$, SplitIt, 1) = CHR$(180) THEN Location = Location + 1: GOTO Jump
IF Location = 0 THEN Word$ = Word$ + MID$(L$, SplitIt, 1)
IF Location = 1 THEN Forign1$ = Forign1$ + MID$(L$, SplitIt, 1)
IF Location = 2 THEN Forign2$ = Forign2$ + MID$(L$, SplitIt, 1)
IF Location = 3 THEN Forign3$ = Forign3$ + MID$(L$, SplitIt, 1)
Jump:
NEXT SplitIt

END SUB

SUB SYS
MakeWindow 1, 1, 21, 80, 4, 6
IF FileNew = 0 THEN SYSTEM
COLOR 6, 4
LOCATE 1, 34: PRINT "Quit WM V0.01"
COLOR 11, 6
LOCATE 5, 2: PRINT STRING$(78, 205)
LOCATE 6, 3: PRINT "file:        .dat"
LOCATE 7, 2: PRINT STRING$(78, 205)
LOCATE 9, 3: PRINT "Leave filename empty to quit without saving!"
LOCATE 10, 3: PRINT "Thank you for using cm-SoFT WordMinder!"
Eingabe 6, 8, 8, file$
IF file$ = "" THEN GOTO FINALEEND
CO2$ = file$ + ".dat"
CO$ = "ren wm.tmp " + file$ + ".dat"
KILL CO2$
SHELL CO$
VocReal$(1) = file$ + ".sc1"
VocReal$(2) = file$ + ".sc2"
VocReal$(3) = file$ + ".sc3"
VocReal$(4) = file$ + ".sc4"
VocReal$(5) = file$ + ".sc5"
FOR CopyReal = 1 TO 5
Core$ = "copy " + Voc$(CopyReal) + " " + VocReal$(CopyReal) + " /Y >nul"
SHELL Core$
NEXT CopyReal
filee$ = file$ + ".dat"
OPEN filee$ FOR INPUT AS #1
CLOSE #1
FINALEEND:
SHELL "if exist wm.tmp del wm.tmp"
SHELL "if exist *.s? del *.s?"
SHELL "CLS"
SYSTEM
END SUB

SUB Weekday (Day$)
'Dieses Programm berechnet die Wochentage nach dem Gregorianischen Kalender.
'Daher stimmen die Tage vor dem 16. und 17. Jahrhundert nicht Åberein.
'Der Gregorianische Kalender wurde zwischen dem 16. u. 17 Jahrhundert
'langsam eingefÅhrt. Heute werden aber schon in guten Lexikas das, mit dem
'Gregorianischen Kalender gÅltige Datum, mit dem alten Datum (Julianischer
'Kalender) umgerechnet.
Tag = VAL(MID$(DATE$, 4, 2))
Monatt = VAL(LEFT$(DATE$, 2))
Jahr = VAL(RIGHT$(DATE$, 4))
DIM monat(12)
        monat(1) = 31       'Jedes Monat wird angegeben, ohne Schaltjahr(365T)
        monat(2) = 28
        monat(3) = 31
        monat(4) = 30
        monat(5) = 31
        monat(6) = 30
        monat(7) = 31
        monat(8) = 31
        monat(9) = 30
        monat(10) = 31
        monat(11) = 30
        monat(12) = 31
     Monatt = Monatt - 1     'Das Monat ist ja noch nicht vollendet!!!!!!
        erg1 = Jahr / 4      'Berechnung der zusÑtlichen Tage (Schaltjahre)
        erg0 = FIX(erg1)     'Alles hinter dem Komma auslassen
        greg1 = Jahr / 100   'Berechnung der entfallenen Schaltjahre (jed. JH)
        greg2 = FIX(greg1)   'Alles hinter dem Komma weg
        greg3 = Jahr / 400   'Aber alle 4 JH doch ein Schaltjahr
        greg4 = FIX(greg3)   'Alles hinter dem Komma auslassen
erg2 = Jahr * 365         'Die Tage der Jahre werden addiert (ohne Schlatjahr)
 FOR month = 1 TO Monatt  'Die Monate werden hochgezÑhlt
  erg3 = erg3 + monat(month)
 NEXT month
erg3 = erg3 + Tag          'Die restlichen Tage werden zu den Monaten addiert
erg4 = erg0 + erg2 + erg3 - greg2 + greg4       'Die Summe aller Ergebnisse
erg5 = erg4 / 7            'Die Wochen werden berechnet
erg5 = FIX(erg5)           'Alles hinter dem Komma auslassen
erg5 = erg5 * 7            'Die vollen Wochen werden berechnet
ergfin = erg4 - erg5       'Alle vollen Wochen werden von der Summe weggezÑhlt
IF greg1 = greg2 THEN IF NOT greg3 = greg4 THEN IF Monatt < 2 THEN erg0 = .5
IF erg1 = erg0 THEN IF Monatt < 2 THEN ergfin = ergfin - 1              '<-|
IF ergfin = -1 THEN ergfin = 6      'Der 366. Tag gilt erst ab dem 29.02.--|
Tag$ = "Fehler"
IF ergfin = 0 THEN Tag$ = "Samstag"
IF ergfin = 1 THEN Tag$ = "Sonntag"
IF ergfin = 2 THEN Tag$ = "Montag"
IF ergfin = 3 THEN Tag$ = "Dienstag"
IF ergfin = 4 THEN Tag$ = "Mittwoch"
IF ergfin = 5 THEN Tag$ = "Donnerstag"
IF ergfin = 6 THEN Tag$ = "Freitag"
Monatt = Monatt + 1                  'FÅr die richtige Anzeige
Day$ = Tag$
END SUB

